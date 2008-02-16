class WorksController < ApplicationController
  skip_before_filter :login_required
  def index
    @selected_button = 'works'
  end
end
