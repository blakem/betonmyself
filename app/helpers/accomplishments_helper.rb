module AccomplishmentsHelper
  def checked_column(record)
    '<img src="/images/blue_check.png" border="0">'
  end
  def completion_date_column(record)
    record.completion_date.strftime("%m/%d/%Y")
  end
end
