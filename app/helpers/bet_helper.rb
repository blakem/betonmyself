module BetHelper
  def price_form_column(record, input_name)
    if record.price.nil?
      value = money_format(BomConstant::DEFAULT_BET)
    else 
      value = money_format(record.price)
    end
    text_field :record, :price, 
      active_scaffold_input_text_options(:name => input_name, :value => value)
  end
  def notes_form_column(record, input_name)
    text_area :record, :notes, 
      active_scaffold_input_text_options(:name => input_name, :cols => 80)
  end
  def due_date_form_column(record, input_name) 
    date_select :record, :due_date, 
    :name => input_name, :order => [:month, :day, :year],
    :start_year => Date.today.year 
  end
end
