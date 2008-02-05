class ExpiredController < ApplicationController
  layout "members"
  def index
    @selected_button = 'play'
  end
end
