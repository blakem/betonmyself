class WelcomeController < ApplicationController
  def index
  end
  skip_before_filter :login_required
end

