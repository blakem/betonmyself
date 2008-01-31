class PurchaseController < ApplicationController
#  include SslRequirement
#  ssl_required :index, :credit
  
  before_filter :load_card
  
  filter_parameter_logging :creditcard
  
  def index
    @selected_button = 'purchase'
  end
  def error
    @selected_button = 'purchase'
  end
  def form
    @selected_button = 'purchase'
    @amount = params[:transaction][:price]
  end
  
  # Use the DirectPayment API
  def credit
    @selected_button = 'purchase'
    render :action => 'index' and return unless @creditcard.valid?
    bill_amount = param[:transaction][price]
    
    @response = paypal_gateway.purchase(bill_amount, @creditcard, 
      :ip => request.remote_ip)
      
    if @response.success?
      @purchase = Purchase.create(:response => @response)
      redirect_to :action => "complete", :id => @purchase
    else
      paypal_error(@response)
    end
  end
  
  # Use the Express Checkout API
  def express
    gateway = paypal_gateway(:paypal_express)
    bill_amount = params['price'].to_i

    @response = gateway.setup_purchase(bill_amount,
      :return_url => url_for(:action => 'express_complete'),
      :cancel_return_url => url_for(:action => 'index'),
      :description => "PayPal Website Payments Pro Guide"
    )

    if @response.success?
      # The useraction=commit in the redirect URL tells PayPal there won't
      # be an additional review step at our site before a charge is made
      @transaction = Transaction.new(
        :user_id => self.current_user.id, 
        :trans_type => BomConstant::TRANSACTION_TYPE_IN,
        :price => bill_amount,
        :token2 => @response.params['token']
      )
      @transaction.save!
      redirect_to "#{gateway.redirect_url_for(@response.params['token'])}&useraction=commit"
    else
      paypal_error(@response)
    end
  end
  
  # PayPal Express redirects from PayPal back to this action with a token
  def express_complete
    @selected_button = 'purchase'
    gateway = paypal_gateway(:paypal_express)
    @details = gateway.details_for(params[:token])

    @transaction = Transaction.find_by_token2(@details.params['token']);
    bill_amount = @transaction.price
    
    if @details.success?
      @response = gateway.purchase(bill_amount, 
        :token => @details.params['token'], 
        :payer_id => @details.params['payer_id'])
      if @response.success?
        @transaction.trans_type = BomConstant::TRANSACTION_TYPE_IN;
        @transaction.save!;
        redirect_to :action => "complete", :id => @transaction
      else
        paypal_error(@response)
      end
    else
      paypal_error(@details)
    end
  end
  
  def complete
    raise ActiveRecord::RecordNotFound unless @purchase = Transaction.find_by_token(params[:id])
  end
  
  protected
  
    def paypal_gateway(gw = :paypal)
      ActiveMerchant::Billing::Base.gateway(gw).new(PAYPAL_API_CREDENTIALS)
    end

    def paypal_error(response)
      @paypal_error = response.message
      render :action => 'error'
    end
    
    def load_card
      @creditcard = ActiveMerchant::Billing::CreditCard.new(params[:creditcard])
    end
  
end
