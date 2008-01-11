class AccomplishmentsController < ApplicationController
  active_scaffold :bet do |config|
    config.label = "Accomplishments"
    config.columns = [:descr, :price, :due_date, :notes]
    list.sorting = {:descr => 'ASC'}
    columns[:descr].label = "Goal"
    columns[:price].label = "Payoff"
  end
end
