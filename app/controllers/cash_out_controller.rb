class CashOutController < ApplicationController
  def authorized?
    logged_in? and not current_user.is_demo
  end
  def index
    @accomplishments = self.current_user.accomplishments
    @selected_button = 'cashout'
  end
  def cash_out
    @selected_button = 'cashout'
    if params['commit'] == "Cancel"
      redirect_to '/'
    end
    @ballance = self.current_user.ballance
    @transaction = Transaction.new(:user_id => self.current_user.id)
    @transaction.price = @ballance
    @transaction.direction = BomConstant::TRANSACTION_DIRECTION_OUT
    @transaction.state = BomConstant::TRANSACTION_STATE_SUCCESS
    @transaction.save!
    log_transaction_out(@transaction)
  end
end
