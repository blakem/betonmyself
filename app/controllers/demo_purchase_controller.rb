class DemoPurchaseController < ApplicationController
  skip_before_filter :login_required
  def index
    @selected_button = 'demo'
  end
end
