class MembersController < ApplicationController
  active_scaffold :bet do |config|
    config.label = "Your Motivational Bets"
#    config.columns = [:name, :phone, :company_type, :comments]
#    list.columns.exclude :comments
#    list.sorting = {:name => 'ASC'}
    columns[:descr].label = "Goal"
  end
end
