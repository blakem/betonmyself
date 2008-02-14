class CashOutController < ApplicationController
  layout "members"
  def authorized?
    members_authorized?
  end
  def index
    @selected_button = 'cashout'
    @user = current_user
  end
  def cash_out
    @selected_button = 'cashout'
    if params['commit'] == "Cancel"
      create_survey
      redirect_to '/'
    else
      create_survey
      @available_cashout_funds = self.current_user.available_cashout_funds
      @cash_out_request = CashOutRequest.new
      suggested_price = compute_suggested_price(@available_cashout_funds)
      @cash_out_request.price = suggested_price
      @cash_out_request.paypal_account = current_user.last_paypal_acct  
      @cash_out_request.method = BomConstant::CASH_OUT_TYPE_PAYPAL
      @user = current_user
    end
  end
  def submit
    @selected_button = 'cashout'
    if params['commit'] == "Cancel"
      redirect_to '/'
    else
      @cash_out_request = CashOutRequest.new(params[:cash_out_request])
      @cash_out_request.user_id = current_user.id
      @cash_out_request.state = BomConstant::CASH_OUT_STATE_PENDING
      @price = params[:cash_out_request]['price']
      @price = @price.sub(/\$/, "")
      @price = @price.to_f * 100
      @cash_out_request.price = @price
      @cash_out_request.save!
      create_transaction_out(@price)
    end
  rescue ActiveRecord::RecordInvalid
    @available_cashout_funds = self.current_user.available_cashout_funds
    @user = current_user
    render :action => 'cash_out'
  end

  protected
    def create_transaction_out(price)
      @transaction = Transaction.new(:user_id => self.current_user.id)
      @transaction.price = price
      @transaction.direction = BomConstant::TRANSACTION_DIRECTION_OUT
      @transaction.state = BomConstant::TRANSACTION_STATE_SUCCESS
      @transaction.save!
      log_transaction_out(@transaction)
    end
    def create_survey
      survey_fields = params[:survey]
      if not survey_fields.nil?
        Survey.new(
          :version => 1,
          :user_id => current_user.id,
          :q1 => survey_fields['q1'],
          :q2 => survey_fields['q2'],
          :q3 => survey_fields['q3']
        ).save!
      end
    end
    def compute_suggested_price(price)
      price = (price/100).to_i
      suggested = 0
      if price > 1000
        suggested_price = 1000
      elsif price > 500
        suggested_price = 500
      elsif price > 300
        suggested_price = 300
      elsif price > 200
        suggested_price = 200
      elsif price > 100
        suggested_price = 100
      elsif price > 50
        suggested_price = 50
      elsif price > 20
        suggested_price = 20
      elsif price > 10
        suggested_price = 10
      elsif price > 5
        suggested_price = 5
      elsif price > 1
        suggested_price = 1
      else 
        suggested_price = 0
      end
      return suggested_price * 100
    end
end
