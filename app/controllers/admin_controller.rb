class AdminController < ApplicationController
  layout "members"
  def index
    @selected_button = 'admin'
  end
end
