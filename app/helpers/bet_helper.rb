module BetHelper
  def descr_form_column(record, input_name)
    options = active_scaffold_input_text_options(
       :name => input_name, 
       :value => record.descr)
    options[:size] = 43
    text_field :record, :descr, options
  end
  def price_form_column(record, input_name)
    if record.price.nil? or record.price < BomConstant::MINIMUM_BET
      value = money_format(BomConstant::DEFAULT_BET)
    else 
      value = money_format(record.price)
    end
    value = '$' + value
    options = active_scaffold_input_text_options(:name => input_name, :value => value) 
    options[:size] = 9
    text_field :record, :price, options
      
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
    '<b>$' + money_format(record.price) + '</b>'
  end
  def render_price_sum(sum)
    '<b>$' + money_format(sum) + '</b>'
  end
  def render_descr_count(count)
    plural = count == 1 ? '' : 's'
    '<b>' + count.to_s + ' Completed Accomplishment' + plural + '!</b>'
  end
  def congrats_column(record)
    record.congrats.nil? ? '' : '<b>' + record.congrats + '</b>'
  end
  def due_date_column(record)
    if record.state == BomConstant::BET_STATE_CURRENT and record.due_date <= Date.today
      '<span class="red_link">Today!</span>'
    elsif record.state == BomConstant::BET_STATE_CURRENT and record.due_date <= Date.today + 1
      'Tomorrow'
    else
      format_date(record.due_date)
    end
  end
end
