class AccomplishmentsController < ApplicationController
  active_scaffold :bet do |config|
    config.columns = [:checked, :descr, :due_date, :completion_date, 
      :price, :congrats]
    config.columns[:price].calculate = :sum
    config.columns[:descr].calculate = :count
    config.show.columns.exclude :checked  
    config.show.columns.add :notes  

    columns[:checked].label = ""
    columns[:descr].label = "Goal"
    columns[:price].label = "Payoff"

    config.actions = [:update, :delete, :show, :list, :search]
    config.delete.link.confirm = 
      'Really?! Deleting this Accomplishment.  Are you Sure?';
    config.columns.add :checked
    config.label = "Accomplishments"
    config.update.columns = [:congrats, :notes]

    list.per_page = BomConstant::RECORDS_PER_PAGE
    list.sorting = {:completion_date => :desc}
  end

  def list
    Bet.authorize_for_user_id(params[:user_id], current_user)
    super
  end

  def do_destroy
    @record = find_if_allowed(params[:id], :destroy)
    @record.state = BomConstant::BET_STATE_DELETED
    @record.save!
    log_bets_delete(@record)
  end
  def do_update
    super
    if successful?
      log_bets_update(@record)
    end
  end
end
