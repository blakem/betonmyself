class ExpiredController < ApplicationController
  layout "members"
  def authorized?
    logged_in? and not current_user.is_demo
  end
  def index
    @selected_button = 'play'
  end
end