class StoryController < ApplicationController
  skip_before_filter :login_required
  def index
    @selected_button = 'story'
  end
end
