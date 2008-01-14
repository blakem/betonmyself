class BetController < ApplicationController
  active_scaffold :bet do |config|
    config.columns = [:descr, :price, :due_date, :notes]
    columns[:descr].label = "Goal"
    columns[:price].label = "Payoff"

    config.action_links.add 'complete', :label => 'Complete', :type => :record
    config.actions.exclude :delete
    config.actions.exclude :search
    config.label = "Current Goals"
    config.update.columns = [:notes]

    list.per_page = BomConstant::RECORDS_PER_PAGE
    list.sorting = {:due_date => 'ASC'}
  end
  def before_create_save(record)
    if not record.price.nil?
      record.price *= 100
    end
  end

  def complete
    do_edit # finds @record
    @record.state = BomConstant::BET_STATE_SUCCESS
    @record.completion_date = Date.today
    @record.save!
  end
end
