class ContactController < ApplicationController
  skip_before_filter :login_required
  def index
    @selected_button = 'contact'
  end
end
