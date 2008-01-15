class BetController < ApplicationController
  active_scaffold :bet do |config|
    config.columns = [:descr, :price, :due_date, :notes]
    columns[:descr].label = "Goal"
    columns[:price].label = "Payoff"

    config.create.label = "Enter Your New Goal"
    config.create.link.label = 'Create New Goal'
    config.delete.link.label = 'Complete'
    config.delete.link.confirm = 'Yay! Click OK to complete task.'
    config.actions = [:create, :delete, :update, :show, :list]
    config.label = "Current Goals"
    config.update.columns = [:notes]

    list.per_page = BomConstant::RECORDS_PER_PAGE
    list.sorting = {:due_date => 'ASC'}
  end
 
  def do_destroy # called by complete...
    do_edit # finds @record
    @record.state = BomConstant::BET_STATE_SUCCESS
    @record.completion_date = Date.today
    @record.save!
  end
  def do_new
    @record = active_scaffold_config.model.new
    @record.price = BomConstant::DEFAULT_BET
    apply_constraints_to_record(@record)
    @record
  end
  def do_create
    price = params[:record]['price']
    price = price.sub(/\$/, "")
    price = price.to_f * 100
    params[:record]['price'] = price
    super
  end
end
