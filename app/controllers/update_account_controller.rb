class UpdateAccountController < ApplicationController
  def authorized?
    members_authorized?
  end
  def index
    @selected_button = 'support'
    @user = self.current_user
  end
  def change
    @selected_button = 'support'
    @user = self.current_user
    @new_email = false
    new_fields = params[:user]
    @user.first_name = new_fields[:first_name]
    @user.last_name = new_fields[:last_name]
    @user.password = new_fields[:password]
    @user.password_confirmation = new_fields[:password_confirmation]
    @user.old_password = new_fields[:old_password]
    @user.old_password_required = 1
    @user.save!
    if @user.email != new_fields[:email]
      @user.new_email = new_fields[:email]
      @user.new_email_activation_code = Digest::SHA1.hexdigest( Time.now.to_s.split(//).sort_by {rand}.join )
      Notifier.deliver_change_email_notification(@user)
      Notifier.deliver_change_email_notification_old_email(@user)
      @user.save!
      @new_email = true
    end
    log_users_update(@user)
  rescue ActiveRecord::RecordInvalid
    render :action => 'index'
  end
  def change_email
    @selected_button = 'support'
    if current_user.new_email_activation_code == params[:id]
      current_user.email = current_user.new_email
      current_user.new_email = nil
      current_user.new_email_activation_code = nil
      current_user.save!
    end
    redirect_to :action => 'index' 
  rescue ActiveRecord::RecordInvalid
    render :action => 'index'
  end
end
