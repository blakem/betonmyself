class CashOutController < ApplicationController
  def index
    @accomplishments = self.current_user.accomplishments
    @selected_button = 'cashout'
  end
  def cash_out
    @selected_button = 'cashout'
    @ballance = self.current_user.ballance
    @transaction = Transaction.new(:user_id => self.current_user.id)
    @transaction.price = @ballance
    @transaction.trans_type = BomConstant::TRANSACTION_TYPE_OUT
    @transaction.save!
    log_transaction_out(@transaction)
  end
end
