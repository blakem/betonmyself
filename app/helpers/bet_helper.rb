module BetHelper
  def price_form_column(record, input_name)
    if record.price.nil? or record.price < BomConstant::MINIMUM_BET
      value = money_format(BomConstant::DEFAULT_BET)
    else 
      value = money_format(record.price)
    end
    value = '$' + value
    text_field :record, :price, 
      active_scaffold_input_text_options(:name => input_name, :value => value)
  end
  def notes_form_column(record, input_name)
    my_text_area(:notes)
  end
  def congrats_form_column(record, input_name)
    my_text_area(:congrats)
  end
  def due_date_form_column(record, input_name) 
    date_select :record, :due_date, {
      :name => input_name, 
      :order => [:month, :day, :year],
      :default => Date.today + 1,
    } 
  end
  def price_column(record)
    return '<b>$' + money_format(record.price) + '</b>'
  end
  def congrats_column(record)
    record.congrats.nil? ? '' : '<b>' + record.congrats + '</b>'
  end
  def due_date_column(record)
    if record.state == BomConstant::BET_STATE_CURRENT and record.due_date <= Date.today
      '<span class="red_link">' + format_date(record.due_date) + '</span>'
    else
      format_date(record.due_date)
    end
  end
end
