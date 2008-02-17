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
    @amount = params[:transaction][:price].to_i
    if not valid_purchase_amount(@amount)
      invalid_price_error(@amount, 'credit')
      return
    end
    @address = BomAddress.new
    @states = get_states
  end
  
  # Use the DirectPayment API
  def credit
    @selected_button = 'purchase'
    @amount = params['price'].to_i
    if not valid_purchase_amount(@amount)
      invalid_price_error(@amount, 'credit')
      return
    else
      @address = BomAddress.new()
      render :action => 'form' and return unless params[:address]
      @address.address1 = params[:address]['address1']
      @address.address2 = params[:address]['address2']
      @address.city = params[:address]['city']
      @address.state = params[:address]['state']
      @address.zip = params[:address]['zip']
      @states = get_states
      valid_address = @address.valid? # need to call valid on both
      valid_cc = @creditcard.valid?   # objects to avoid lazy evaluation
      custom_cc_validation
      if not valid_address or not valid_cc
        render :action => 'form'
        return
      end

      billing_address = { 
        :name     => @creditcard.first_name + " " + @creditcard.last_name,
        :address1 => @address.address1,
        :address2 => @address.address2,
        :city     => @address.city,
        :state    => @address.state,
        :zip      => @address.zip,
        :country  => 'US',
        :phone    => ''
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
        @transaction = Transaction.new
        @transaction.user_id = current_user.id
        @transaction.price = @amount
        paypal_error(@transaction, @response, 'credit response fail')
        return
      end
    end
  end
  
  # Use the Express Checkout API
  def express
    gateway = paypal_gateway(:paypal_express)
    bill_amount = params['price'].to_i
    if not valid_purchase_amount(bill_amount)
      invalid_price_error(bill_amount, 'express')
      return
    else
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
        paypal_error(@transaction, @response, 'express response failure')
        return
      end
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
        paypal_error(@transaction, @response, 'express complete error')
        return
      end
    else
      paypal_error(@transaction, @details, 'express complete detail error')
      return
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
    def invalid_price_error(price, stage)
      @transaction = Transaction.new
      @transaction.user_id = current_user.id
      @transaction.price = price
      @response = PseudoResponse.new
      @response.message = 'Price is invalid';
      paypal_error(@transaction, @response, stage + ' invalid price')
    end
    def custom_cc_validation
      if not @creditcard.verification_value.blank?
        if @creditcard.verification_value !~ /^\d{3}$/
          @creditcard.errors.add(:verification_value, 'must be 3 digits')
        end
      end
      if not @creditcard.number.blank?
        if @creditcard.number !~ /^\d{16}$/
          @creditcard.errors.add(:number, 'must be 16 digits')
        end
      end
      if not @address.zip.blank?
        if @address.zip !~ /^\d{5}(-\d{4})?$/ and @address.zip !~ /^[ABCEGHJKLMNPRSTVXY]\d[A-Z] *\d[A-Z]\d?$/i
          @address.errors.add(:zip, 'is invalid, it must be five digits')
        end
      end
    end

    def paypal_error(transaction, obj, stage)
      log_transaction_fail(transaction, obj, stage)
      @paypal_error = obj.message
      @selected_button = 'purchase'
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
