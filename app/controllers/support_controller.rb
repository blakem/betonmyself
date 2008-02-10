class SupportController < ApplicationController
  def authorized?
    members_authorized?
  end
  def index
    @selected_button = 'support'
  end
end
