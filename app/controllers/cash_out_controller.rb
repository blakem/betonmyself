class CashOutController < ApplicationController
  def index
    @accomplishments = self.current_user.accomplishments
  end
  def cash_out
    @ballance = self.current_user.ballance
    @transaction = Transaction.new(:user_id => self.current_user.id)
    @transaction.price = @ballance
    @transaction.trans_type = BomConstant::TRANSACTION_TYPE_OUT
    @transaction.save!
  end
end
