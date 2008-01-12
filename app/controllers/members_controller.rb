class MembersController < ApplicationController
  def index
    @user = User.find(session[:user_id]);
    @cash = sprintf "%.02f", @user.ballance / 100;
  end
end
