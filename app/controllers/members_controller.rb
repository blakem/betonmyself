class MembersController < ApplicationController
  active_scaffold :bet do |config|
    config.label = "Your Motivational Bets"
    config.columns = [:descr, :amount, :due_date, :notes]
    list.sorting = {:descr => 'ASC'}
    columns[:descr].label = "Goal"
    columns[:amount].label = "Payoff"
  end
  def conditions_for_collection
#    ['amount IN (?)', ['34']]
  end
end
