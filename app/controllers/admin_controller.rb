class AdminController < ApplicationController
  layout "members"
  def authorized?
    members_authorized? and current_user.is_admin
  end
  def index
    @selected_button = 'admin'
  end
end
