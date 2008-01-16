# Methods added to this helper will be available to all templates in the application.
module ApplicationHelper
  include BomUtility
  def my_text_area(field)
    text_area :record, field, {
      :cols => 40,
      :style => 'letter-spacing: -1px;border: solid 1px #1F7F00;' +
                'font: bold 16px arial'
    }
  end
end
