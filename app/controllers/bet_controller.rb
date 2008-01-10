class BetController < ApplicationController
  active_scaffold :bet do |config|
    config.label = "Current Goals"
    config.columns = [:descr, :amount, :due_date, :notes]
    list.sorting = {:descr => 'ASC'}
    columns[:descr].label = "Goal"
    columns[:amount].label = "Payoff"
  end
end
