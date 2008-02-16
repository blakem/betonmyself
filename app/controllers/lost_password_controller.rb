class LostPasswordController < ApplicationController
  skip_before_filter :login_required
  def index
    @selected_button = 'members'
  end
  def email
    @selected_button = 'members'
    @user = User.new
    email = params[:user]['email']
    if email.nil? or email == ""
      @user.errors.add(:email, "is required")
    else
      found_user = User.find_by_email(params[:user]['email'])
      if found_user.nil?
        @user.errors.add(:email, "address does not match our records")
      end
    end

    if @user.errors.any?
      @user.email = params[:user]['email']
      render :action => 'index'
    else 
      found_user = User.find_by_email(params[:user]['email'])
      found_user.reset_password_token = make_random_token(12)
      found_user.reset_password_token_expires_at = Time.now.utc + 1.day
      found_user.save!
      Notifier.deliver_reset_password(found_user)
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
