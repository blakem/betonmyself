class UsersController < ApplicationController
  skip_before_filter :login_required
  def new
    @selected_button = 'signup'
  end

  def create
    @selected_button = 'signup'
    cookies.delete :auth_token
    # protects against session fixation attacks, wreaks havoc with 
    # request forgery protection.
    # uncomment at your own risk
    # reset_session
    @user = User.new(params[:user])
    @user.role = BomConstant::ROLE_TYPE_USER
    @user.save!
    Notifier.deliver_signup_notification(@user)
    log_users_create(@user)
    flash[:notice] = "Thanks for signing up!"
  rescue ActiveRecord::RecordInvalid
    render :action => 'new'
  end

  def activate
    self.current_user = params[:id].blank? ? :false : User.find_by_activation_code(params[:id])
    if logged_in? && !current_user.active?
      current_user.activate
      if current_user.pending?
        Notifier.deliver_activation(current_user) 
        Notifier.deliver_sms_user_activate(current_user)
      end
      flash[:notice] = "Signup complete!"
    end
    redirect_back_or_default('/intro')
  end
end
