class IntroController < ApplicationController
  def authorized?
    members_authorized?
  end
  def index
    @selected_button = 'intro'
  end
end
