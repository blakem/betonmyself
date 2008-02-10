class ExpiredController < ApplicationController
  layout "members"
  def authorized?
    members_authorized?
  end
  def index
    @selected_button = 'play'
  end
end
