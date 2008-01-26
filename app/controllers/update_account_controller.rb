class UpdateAccountController < ApplicationController
  def index
    @selected_button = 'support'
    @user = self.current_user
  end
  def change
    @selected_button = 'support'
    @user = self.current_user
    next_page = '/members'
    new_fields = params[:user]
    if (new_fields[:password] != new_fields[:password_confirm]) 
      next_page = '/update_account'
    else
      @user.first_name = new_fields[:first_name]
      @user.last_name = new_fields[:last_name]
      @user.email = new_fields[:email]
      @user.password = new_fields[:password]
      @user.save!
    end
    redirect_to(next_page)
  end
end
