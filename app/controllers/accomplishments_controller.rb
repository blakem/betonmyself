class AccomplishmentsController < ApplicationController
  active_scaffold :bet do |config|
    config.columns = [:checked, :descr, :due_date, :completion_date, 
      :price, :congrats]
    config.show.columns.exclude :checked  
    config.show.columns.add :notes  

    columns[:checked].label = ""
    columns[:descr].label = "Goal"
    columns[:price].label = "Payoff"

    config.actions = [:update, :show, :list, :search]
    config.columns.add :checked
    config.label = "Accomplishments"
    config.update.columns = [:notes, :congrats]

    list.per_page = BomConstant::RECORDS_PER_PAGE
    list.sorting = {:completion_date => 'DESC'}
  end
end
