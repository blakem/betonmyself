class IntroController < ApplicationController
  def authorized?
    members_authorized?
  end
  def index
    @selected_button = 'intro'
    @loan_amount = current_user.total_loans_in
  end
end
