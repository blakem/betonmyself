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
    new_fields = params[:user]
    @user.first_name = new_fields[:first_name]
    @user.last_name = new_fields[:last_name]
    @user.email = new_fields[:email]
    @user.password = new_fields[:password]
    @user.password_confirmation = new_fields[:password_confirmation]
    @user.old_password = new_fields[:old_password]
    @user.old_password_required = 1
    @user.save!
    log_users_update(@user)
    redirect_to('/')
  rescue ActiveRecord::RecordInvalid
    render :action => 'index'
  end
end
