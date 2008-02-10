class HelpController < ApplicationController
  def authorized?
    members_authorized?
  end
  def index
    @selected_button = 'help'
  end
end
