class UpdateAccountController < ApplicationController
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
    @user.save!
    redirect_to('/members')
  rescue ActiveRecord::RecordInvalid
    render :action => 'index'
  end
end
