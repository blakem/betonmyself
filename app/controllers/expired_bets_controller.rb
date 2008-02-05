class ExpiredBetsController < ApplicationController
  active_scaffold :bet do |config|
    config.columns = [:descr, :price, :due_date, :notes]
    config.columns[:price].calculate = :sum

    columns[:descr].label = "Goal"
    columns[:price].label = "Payoff"
    columns[:notes].label = "Notes / Next Action"

    config.actions = [:delete, :show, :list]
    config.delete.link.confirm = 
      'Delete this expired goal. Are you Sure?';
    config.label = "Expired Goals"

    list.per_page = BomConstant::RECORDS_PER_PAGE
    list.sorting = {:due_date => 'DESC'}
  end
  def show
    Bet.authorize_for_user(params[:id], current_user)
    super
  end
  def row
    Bet.authorize_for_user(params[:id], current_user)
    super
  end
  def list
    Bet.authorize_for_user_id(params[:user_id], current_user)
    super
  end

  def do_destroy
    Bet.authorize_for_user(params[:id], current_user)
    @record = find_if_allowed(params[:id], :destroy)
    @record.state = BomConstant::BET_STATE_FAILED_DELETED
    @record.save!
    log_bets_delete_failed(@record)
  end
end
