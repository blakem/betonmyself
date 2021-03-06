class ExpiredBetsController < ApplicationController
  active_scaffold :bet do |config|
    config.columns = [:descr, :price, :due_date, :notes]
    config.columns[:price].calculate = :sum

    columns[:descr].label = "Goal"
    columns[:price].label = "Cost"
    columns[:notes].label = "Notes / Next Action"

    config.actions = [:show, :list]
    config.label = "Expired Goals"

    list.per_page = BomConstant::RECORDS_PER_PAGE
    list.sorting = [{ :due_date => :desc}, {:created_at => :asc}]
  end

  def list
    Bet.authorize_for_user_id(params[:user_id], current_user)
    super
  end

  def do_destroy
    @record = find_if_allowed(params[:id], :destroy)
    log_bets_delete_failed(@record)
  end
end
