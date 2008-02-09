class LostPasswordController < ApplicationController
  skip_before_filter :login_required
  def index
    @selected_button = 'members'
  end
  def email
    @selected_button = 'members'
    email = params[:user]['email']
    if email.nil? or email == ""
      error = "Email cannot be blank"
    else
      user = User.find_by_email(params[:user]['email'])
      if user.nil?
        error = "Can't find Email Address"
      end
    end

    if error
      flash.now[:error] = error
      render :action => 'index'
    else 
      user = User.find_by_email(params[:user]['email'])
      user.reset_password_token = make_random_token(12)
      user.reset_password_token_expires_at = Time.now.utc + 1.day
      user.save!
      Notifier.deliver_reset_password(user)
      redirect_to :action => 'sent'
    end
  end
  def sent
    @selected_button = 'members'
  end
  def reset
    @selected_button = 'members'
    user = User.find_by_reset_password_token(params[:id])
    if ((not user.nil?) and (user.reset_password_token_expires_at > Time.now.utc))
      @password = make_random_token(8)
      user.password = @password
      user.password_confirmation = @password
      user.reset_password_token = nil
      user.reset_password_token_expires_at = nil
      user.save!
    else
      redirect_to :action => 'error'
    end
  end
  def error
    @selected_button = 'members'
  end
end
