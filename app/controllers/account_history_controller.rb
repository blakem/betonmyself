class AccountStatusController < ApplicationController
  layout "members"
  def index
    @selected_button = 'support'
    @user = current_user
  end
end
