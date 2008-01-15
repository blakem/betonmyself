class AccomplishmentsController < ApplicationController
  active_scaffold :bet do |config|
    config.columns = [:checked, :descr, :price, :completion_date, 
      :due_date, :notes]
    config.show.columns.exclude :checked  

    columns[:checked].label = ""
    columns[:descr].label = "Goal"
    columns[:price].label = "Payoff"

    config.actions = [:update, :show, :list, :search]
    config.columns.add :checked
    config.label = "Accomplishments"
    config.update.columns = [:notes]

    list.per_page = BomConstant::RECORDS_PER_PAGE
    list.sorting = {:completion_date => 'DESC'}
  end
end
