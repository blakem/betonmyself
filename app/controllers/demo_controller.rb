class DemoController < ApplicationController
  skip_before_filter :login_required
  layout "members"
  def index
    init
    @selected_button = 'demo'
    @ballance_text = self.account_ballance_text
    @show_expired = current_user.failed_bets.length
  end
  def get_account_ballance
    init
    render :text => self.account_ballance_text
  end
  protected
  def account_ballance_text
    "You have <b>$" + money_format(current_user.ballance) + "</b> to spend<br>"
  end
  def init
    self.current_user = User.find_by_role(BomConstant::ROLE_TYPE_DEMO)
  end
end
