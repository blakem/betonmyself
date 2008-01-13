module BetHelper
   def price_form_column(record, input_name)
     eid = params[:eid]
     return '<input autocomplete="off" class="price-input text-input" ' +
            'id="record_price_' + eid + '" name="record[price]" size="20" ' +
            'type="text" value="' + 
            money_format(BomConstant::DEFAULT_BET) +
            '">'
   end
end
