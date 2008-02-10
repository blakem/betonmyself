class DemoPurchaseController < ApplicationController
  skip_before_filter :login_required
  def index
    init
    @selected_button = 'demo'
  end
  protected
  def init
    self.current_user = User.find_by_role(BomConstant::ROLE_TYPE_DEMO)
  end
end
