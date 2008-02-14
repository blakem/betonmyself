class MembersController < ApplicationController
  def authorized?
    members_authorized?
  end
  def index
    @selected_button = 'play'
    @balance_text = self.account_balance_text
    @show_expired = current_user.failed_bets.length 
  end
  def get_account_balance
    render :text => self.account_balance_text
  end
  protected
  def account_balance_text
    "You have <b>$" + money_format(current_user.balance) + "</b> to spend<br>"
  end
end
