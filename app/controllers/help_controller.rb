class HelpController < ApplicationController
  def authorized?
    logged_in? and not current_user.is_demo
  end
  def index
    @selected_button = 'help'
  end
end
