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
    create_loan
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
        log_users_activate(current_user)
      end
      flash[:notice] = "Signup complete!"
    end
    self.current_user = nil
    redirect_back_or_default('/intro')
  end

  protected
    def create_loan
      @loan_in = Transaction.new(:user_id => @user.id)
      @loan_in.price = BomConstant::INITIAL_LOAN_AMOUNT
      @loan_in.direction = BomConstant::TRANSACTION_DIRECTION_IN
      @loan_in.state = BomConstant::TRANSACTION_STATE_SUCCESS
      @loan_in.trans_type = BomConstant::TRANSACTION_TYPE_BOM_LOAN
      @loan_in.fee_amount = 0
      @loan_in.save!
      @loan_out = Transaction.new(:user_id => @user.id)
      @loan_out.price = BomConstant::INITIAL_LOAN_AMOUNT
      @loan_out.direction = BomConstant::TRANSACTION_DIRECTION_OUT
      @loan_out.state = BomConstant::TRANSACTION_STATE_SUCCESS
      @loan_out.trans_type = BomConstant::TRANSACTION_TYPE_BOM_LOAN
      @loan_out.save!
      log_transaction_loan(@loan_in)
    end
end
