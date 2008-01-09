class SignupController < ApplicationController
  def index
  end
  def create
    @user = User.new(params[:user])
    User.transaction do
      @user.save!
      redirect_to :controller => 'members', :id => @user
    end
  end
end
