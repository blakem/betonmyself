class PurchaseController < ApplicationController
#  include SslRequirement
#  ssl_required :index, :credit
  
  before_filter :load_card
  filter_parameter_logging :creditcard
  def authorized?
    members_authorized?
  end

  def index
    @selected_button = 'purchase'
  end
  def error
    @selected_button = 'purchase'
  end
  def form
    @selected_button = 'purchase'
    @amount = params[:transaction][:price]
    @address = BomAddress.new
    @states = get_states
  end
  
  # Use the DirectPayment API
  def credit
    @selected_button = 'purchase'
    @amount = params['price'].to_i
    render :action => 'form' and return unless @creditcard.valid?

billing_address = { 
    :name     => "John Smith",
    :address1 => '123 First St.',
    :address2 => '',
    :city     => 'Los Angeles',
    :state    => 'CA',
    :country  => 'US',
    :zip      => '90068',
    :phone    => '310-555-1234'
}    

    @response = paypal_gateway.purchase(@amount, @creditcard, 
      :ip => request.remote_ip, :billing_address => billing_address)
    log_paypal_obj('Credit Response', @response)
      
    if @response.success?
      @transaction = Transaction.new(:response => @response)
      @transaction.user_id = self.current_user.id
      @transaction.trans_type = BomConstant::TRANSACTION_TYPE_PAYPAL_CREDIT
      @transaction.direction = BomConstant::TRANSACTION_DIRECTION_IN
      @transaction.state = BomConstant::TRANSACTION_STATE_SUCCESS
      @transaction.price = @amount
      @transaction.save!
      redirect_to :action => "complete", :id => @transaction
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
    log_paypal_obj('Express Response', @response)
    if @response.success?
      # The useraction=commit in the redirect URL tells PayPal there won't
      # be an additional review step at our site before a charge is made
      @transaction = Transaction.new(
        :user_id => self.current_user.id, 
        :trans_type => BomConstant::TRANSACTION_TYPE_PAYPAL_EXPRESS,
        :direction => BomConstant::TRANSACTION_DIRECTION_IN,
        :state => BomConstant::TRANSACTION_STATE_INIT,
        :price => bill_amount,
        :remote_token => @response.params['token']
      )
      @transaction.save!
      log_transaction_init(@transaction)
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
    log_paypal_obj('Express Complete Details', @details)

    @transaction = Transaction.find_by_remote_token(@details.params['token']);
    @transaction.name = @details.params['name']
    @transaction.state_or_province = @details.params['state_or_province']
    @transaction.payer_country = @details.params['payer_country']
    @transaction.address_owner = @details.params['address_owner']
    @transaction.postal_code = @details.params['postal_code']
    @transaction.payer = @details.params['payer']
    @transaction.payer_status = @details.params['payer_status']
    @transaction.save!
    bill_amount = @transaction.price
    
    if @details.success?
      @response = gateway.purchase(bill_amount, 
        :token => @details.params['token'], 
        :payer_id => @details.params['payer_id'])
      log_paypal_obj('Express Complete Response', @response)
      if @response.success?
        @transaction.state = BomConstant::TRANSACTION_STATE_SUCCESS
        if @response.params['fee_amount']
          @transaction.fee_amount = (@response.params['fee_amount'].to_f * 100).to_i
        end
        if @response.params['gross_amount']
          @transaction.gross_amount = (@response.params['gross_amount'].to_f * 100).to_i
        end
        if @response.params['tax_amount']
          @transaction.tax_amount = (@response.params['tax_amount'].to_f * 100).to_i
        end
        @transaction.save!;
        log_transaction_in(@transaction)
        redirect_to :action => "complete", :id => @transaction
      else
        log_transaction_fail(@transaction, @response)
        paypal_error(@response)
      end
    else
      log_transaction_fail(@transaction, @details)
      paypal_error(@details)
    end
  end
  
  def complete
    @selected_button = 'purchase'
    raise ActiveRecord::RecordNotFound unless @transaction = Transaction.find_by_token(params[:id])
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

    def get_states
      [
       ["Alabama", "AL"],
       ["Alaska", "AK"],
       ["Arizona", "AZ"],
       ["Arkansas", "AR"],
       ["California", "CA"],
       ["Colorado", "CO"],
       ["Connecticut", "CT"],
       ["Delaware", "DE"],
       ["District Of Columbia", "DC"],
       ["Florida", "FL"],
       ["Georgia", "GA"],
       ["Hawaii", "HI"],
       ["Idaho", "ID"],
       ["Illinois", "IL"],
       ["Indiana", "IN"],
       ["Iowa", "IA"],
       ["Kansas", "KS"],
       ["Kentucky", "KY"],
       ["Louisiana", "LA"],
       ["Maine", "ME"],
       ["Maryland", "MD"],
       ["Massachusetts", "MA"],
       ["Michigan", "MI"],
       ["Minnesota", "MN"],
       ["Mississippi", "MS"],
       ["Missouri", "MO"],
       ["Montana", "MT"],
       ["Nebraska", "NE"],
       ["Nevada", "NV"],
       ["New Hampshire", "NH"],
       ["New Jersey", "NJ"],
       ["New Mexico", "NM"],
       ["New York", "NY"],
       ["North Carolina", "NC"],
       ["North Dakota", "ND"],
       ["Ohio", "OH"],
       ["Oklahoma", "OK"],
       ["Oregon", "OR"],
       ["Pennsylvania", "PA"],
       ["Rhode Island", "RI"],
       ["South Carolina", "SC"],
       ["South Dakota", "SD"],
       ["Tennessee", "TN"],
       ["Texas", "TX"],
       ["Utah", "UT"],
       ["Vermont", "VT"],
       ["Virginia", "VA"],
       ["Washington", "WA"],
       ["West Virginia", "WV"],
       ["Wisconsin", "WI"],
       ["Wyoming", "WY"]
      ]
    end
end
