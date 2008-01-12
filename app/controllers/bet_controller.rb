class BetController < ApplicationController
  active_scaffold :bet do |config|
    config.columns = [:descr, :price, :due_date, :notes]
    columns[:descr].label = "Goal"
    columns[:price].label = "Payoff"

    config.actions.exclude :delete
    config.actions.exclude :search
    config.label = "Current Goals"
    config.update.columns = [:notes]

    list.sorting = {:descr => 'ASC'}
  end
end
