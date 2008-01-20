class FaqController < ApplicationController
  skip_before_filter :login_required
  def index
    @selected_button = 'faq'
  end
end
