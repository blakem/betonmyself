# This controller handles the login/logout function of the site.  
class SessionsController < ApplicationController
  skip_before_filter :login_required
  def new
    @selected_button = 'members'
  end

  def create
    @selected_button = 'members'
    self.current_user = User.authenticate(params[:user]['login'], params[:user]['password'])
    if logged_in?
      if params[:user]['remember_me'] == "1"
        self.current_user.remember_me
        cookies[:auth_token] = { :value => self.current_user.remember_token , :expires => self.current_user.remember_token_expires_at }
      end
      redirect_back_or_default(BomConstant::MEMBERS_LINK)
    else
      @user = PseudoUser.new(params[:user])
      @user.errors.add(:login, 'failed authenticaion')
      @user.errors.add(:password, 'failed authenticaion')
      render :action => 'new'
    end
  end

  def destroy
    self.current_user.forget_me if logged_in?
    cookies.delete :auth_token
    reset_session
    flash[:notice] = "You have been logged out."
    redirect_to BomConstant::WWW_LINK
  end
end
