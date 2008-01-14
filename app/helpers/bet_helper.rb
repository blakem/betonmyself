module BetHelper
   def price_form_column(record, input_name)
     eid = params[:eid]
     if record.price.nil?
       value = money_format(BomConstant::DEFAULT_BET)
     else 
       value = money_format(record.price * 100)
     end
     return '<input autocomplete="off" class="price-input text-input" ' +
            'id="record_price_' + eid + '" name="record[price]" size="20" ' +
            'type="text" value="' + value + '">'
   end
   def notes_form_column(record, input_name)
     text_area :record, :notes, :name => input_name, :cols => 80
   end
   def due_date_form_column(record, input_name) 
     date_select :record, :due_date, 
       :name => input_name, :order => [:month, :day, :year],
       :start_year => Date.today.year 
   end
end
