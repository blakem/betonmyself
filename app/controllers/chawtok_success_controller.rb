class ChawtokSuccessController < ApplicationController
  skip_before_filter :login_required
  def index
    Notifier.deliver_sms_chawtok_success
    redirect_to 'http://www.chawtok.com/success.html'
  end
end
