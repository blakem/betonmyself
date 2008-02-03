class DemoController < ApplicationController
  skip_before_filter :login_required
  layout "members"
  def index
    init
    @ballance_text = self.account_ballance_text(@user)
    @selected_button = 'demo'
  end
  def get_account_ballance
    render :text => self.account_ballance_text(User.find(self.current_user.id))
  end
  protected
  def account_ballance_text (user)
    "You have <b>$" + money_format(user.ballance) + "</b> to spend<br>"
  end
  def init
    @user = User.find_by_role(BomConstant::ROLE_TYPE_DEMO)
    self.current_user = @user
  end
end
