class SummarizeTransactionsController < ApplicationController
  def authorized?
    members_authorized?
  end
  def index
    @selected_button = 'support'
  end
  def email
    @selected_button = 'support'
    Notifier.deliver_summarize_transactions(current_user)
  end
end
