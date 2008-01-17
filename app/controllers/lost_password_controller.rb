class LostPasswordController < ApplicationController
  skip_before_filter :login_required
  def index
    @selected_button = 'members'
  end
end
