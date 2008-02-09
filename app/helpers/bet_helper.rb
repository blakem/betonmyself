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
  def descr_form_column(record, input_name)
    value = record.descr.blank? ? '' : ' value="' + h(record.descr) + '"'
    if record.descr == record.descr_orig or record.descr_orig.blank?
      original = ''
    else
      original = 'Was originally: "<em>' + h(record.descr_orig) + '"</em><br>'
    end
    original + '<input autocomplete="off" class="text-input" id="record_descr" name="record[descr]" size="43" type="text"' + value + '>'
  end
  def descr_column(record)
    log_object(record)
    if record.descr == record.descr_orig or record.descr_orig.blank?
      h(record.descr)
    else
      h(record.descr) + ' (Was originally: <em>' + h(record.descr_orig) + '</em>)'
    end
  end
  def due_date_form_column(record, input_name) 
    is_editable = (record.created_at and (1.hour.ago.utc < record.created_at))
    if (((not record.due_date) or is_editable) or current_user.is_admin)
      start_date = record.due_date.nil? ? Date.today : record.due_date
      field = calendar_date_select_tag "record[due_date]", 
        start_date.strftime("%B %d, %Y"), 
        :id => 'record_due_date',
        :year_range => [0.years.ago, 12.years.from_now],
        :class => 'text-input',
        :month_year => "label"
      if record.due_date and is_editable
        field = field + " <em>(due date is only editable for 1 hour)</em>"
      end
      field
    else
      "<b>" + record.due_date.strftime("%B %d, %Y") + "</b>"
    end
  end
  def price_column(record)
    '<b>$' + money_format(record.price) + '</b>'
  end
  def render_price_sum(sum)
    sum.nil? ? '' : '<b>$' + money_format(sum) + '</b>'
  end
  def render_descr_count(count)
    plural = count == 1 ? '' : 's'
    count == 0 ? '' : '<b>' + count.to_s + ' Completed Accomplishment' + plural + '!</b>'
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
