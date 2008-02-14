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
      @cash_out_request.price = 0
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
end
