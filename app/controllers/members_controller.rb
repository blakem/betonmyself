class MembersController < ApplicationController
  def authorized?
    members_authorized?
  end
  def index
    @selected_button = 'play'
    @ballance_text = self.account_ballance_text
    @show_expired = current_user.failed_bets.length 
  end
  def get_account_ballance
    render :text => self.account_ballance_text
  end
  protected
  def account_ballance_text
    "You have <b>$" + money_format(current_user.ballance) + "</b> to spend<br>"
  end
end
