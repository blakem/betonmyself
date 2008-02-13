class AccountStatusController < ApplicationController
  def index
    @selected_button = 'support'
    @user = current_user
  end
end
