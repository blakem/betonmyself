class PurchaseController < ApplicationController
  def index
    @selected_button = 'purchase'
  end
  def create
    @transaction = Transaction.new(
      :price => params[:transaction]['price'],
      :user_id => self.current_user.id,
      :trans_type => BomConstant::TRANSACTION_TYPE_IN
    )
    @transaction.save!
    log_transaction(@transaction)
    redirect_to('/members')
  end
end
