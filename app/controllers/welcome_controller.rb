class WelcomeController < ApplicationController
  skip_before_filter :login_required
  def index
    @selected_button = 'welcome'
  end
end

