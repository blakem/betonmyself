class AccomplishmentsController < ApplicationController
  active_scaffold :bet do |config|
    config.columns = [:checked, :descr, :price, :due_date, :notes]

    columns[:checked].label = ""
    columns[:descr].label = "Goal"
    columns[:price].label = "Payoff"
    columns[:price].label = "Payoff"

    config.actions.exclude :create
    config.actions.exclude :delete
    config.columns.add :checked
    config.label = "Accomplishments"
    config.update.columns = [:notes]

    list.sorting = {:descr => 'ASC'}
  end
end
