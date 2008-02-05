class DemoExpiredController < ApplicationController
  layout "members"
  def index
    @selected_button = 'demo'
  end
end
