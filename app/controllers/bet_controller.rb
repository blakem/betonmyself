class BetController < ApplicationController
  active_scaffold :bet do |config|
    config.columns = [:descr, :price, :due_date, :notes]
    config.columns[:price].calculate = :sum
    columns[:descr].label = "Goal"
    columns[:price].label = "Payoff"
    columns[:notes].label = "Notes / Next Action"

    config.action_links.add 'complete', :label => 'Complete', :type => :record
    config.action_links['complete'].inline = false;
    config.create.label = "Enter Your New Goal"
    config.create.link.label = 'Create New Goal'
    config.actions = [:create, :update, :show, :list]
    config.label = "Current Goals"
    # config.update.columns = [:descr, :notes, :due_date]
    config.update.columns = [:descr, :notes]

    list.per_page = BomConstant::RECORDS_PER_PAGE
    list.sorting = {:due_date => 'ASC'}
  end

  def list
    Bet.authorize_for_user_id(params[:user_id], current_user)
    super
  end

  def complete
    @selected_button = self.current_user.is_demo ? 'demo' : 'play'
    do_edit
  end
  def complete_submit
    if params['commit'] == "Complete Task"
      do_edit # finds @record
      @record.state = BomConstant::BET_STATE_SUCCESS
      @record.congrats = params[:bet]['congrats']
      @record.completion_date = Date.today
      @record.save!
      log_bets_complete(@record)
    end
    redirect_to self.current_user.is_demo ? '/demo' : '/'
  end
  def do_new
    @record = active_scaffold_config.model.new
    price = self.current_user.ballance
    if price > BomConstant::DEFAULT_BET 
      price = BomConstant::DEFAULT_BET
    end
    @record.price = price
    apply_constraints_to_record(@record)
    @record
  end
  def do_create
    price = params[:record]['price']
    price = price.sub(/\$/, "")
    price = price.to_f * 100
    params[:record]['price'] = price
    super
    if successful?
      @record.descr_orig = @record.descr
      @record.save!
      log_bets_create(@record)
    end
  end
  def do_update
    super
    if successful?
      log_bets_update(@record)
    end
  end
end
