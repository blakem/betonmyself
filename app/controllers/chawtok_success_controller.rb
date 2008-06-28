class ChawtokSuccessController < ApplicationController
  skip_before_filter :login_required
  def index
    log_chawtok_success(params)
    redirect_to 'http://www.chawtok.com/success.html'
  end
end
