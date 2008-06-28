class ChawtokSuccessController < ApplicationController
  skip_before_filter :login_required
  def index
    redirect_to 'http://www.chawtok.com/success.html'
  end
end
