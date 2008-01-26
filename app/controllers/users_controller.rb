class UsersController < ApplicationController
  skip_before_filter :login_required
  def new
    @selected_button = 'signup'
  end

  def create
    cookies.delete :auth_token
    # protects against session fixation attacks, wreaks havoc with 
    # request forgery protection.
    # uncomment at your own risk
    # reset_session
    @user = User.new(params[:user])
    @user.save!
    self.current_user = @user

    @transaction = Transaction.new(:user_id => self.current_user.id)
    @transaction.price = BomConstant::INITIAL_BUY_IN
    @transaction.trans_type = BomConstant::TRANSACTION_TYPE_IN
    @transaction.save!

    redirect_back_or_default('/intro')
    flash[:notice] = "Thanks for signing up!"
  rescue ActiveRecord::RecordInvalid
    render :action => 'new'
  end

end
