module UsersHelper
 include BomUtility
 def price_column(record)
    return '<b>$' + money_format(record.price) + '</b>'
 end
end
