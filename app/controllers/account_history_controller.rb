class AccountHistoryController < ApplicationController
  layout "members"
  def authorized?
    members_authorized?
  end
  def index
    @selected_button = 'support'
    @user = current_user
  end
end
