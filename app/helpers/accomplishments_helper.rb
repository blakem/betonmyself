module AccomplishmentsHelper
  def checked_column(record)
    '&radic;&radic;&radic;'
  end
  def completion_date_column(record)
    record.completion_date.strftime("%m/%d/%Y")
  end
end
