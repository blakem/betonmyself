class SummarizeTransactionsController < ApplicationController
  def authorized?
    logged_in? and not current_user.is_demo
  end
  def index
    @selected_button = 'support'
  end
  def email
    @selected_button = 'support'
    Notifier.deliver_summarize_transactions(current_user)
  end
end
