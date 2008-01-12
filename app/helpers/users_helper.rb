module UsersHelper
 def price_column(record)
    return sprintf "<b>$%.02f</b>", record.price / 100
 end
end
