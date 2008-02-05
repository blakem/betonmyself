class MembersController < ApplicationController
  def authorized?
    logged_in? and not current_user.is_demo
  end
  def index
    @user = User.find(session[:user_id])
    @ballance_text = self.account_ballance_text(@user)
    @selected_button = 'play'
    @show_expired = current_user.failed_bets 
  end
  def get_account_ballance
    render :text => self.account_ballance_text(User.find(session[:user_id]))
  end
  protected
  def account_ballance_text (user)
    "You have <b>$" + money_format(user.ballance) + "</b> to spend<br>"
  end
end
