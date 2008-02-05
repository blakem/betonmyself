class MembersController < ApplicationController
  def authorized?
    logged_in? and not current_user.is_demo
  end
  def index
    @ballance_text = self.account_ballance_text
    @selected_button = 'play'
    @show_expired = current_user.failed_bets 
  end
  def get_account_ballance
    render :text => self.account_ballance_text
  end
  protected
  def account_ballance_text
    "You have <b>$" + money_format(current_user.ballance) + "</b> to spend<br>"
  end
end
