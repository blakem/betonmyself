class MembersController < ApplicationController
  def index
    @user = User.find(session[:user_id])
    @cash = money_format(@user.ballance)
  end
end
