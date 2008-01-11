class MembersController < ApplicationController
  def index
    @user = User.find(session[:user_id]);
    @cash = @user.ballance
  end
end
