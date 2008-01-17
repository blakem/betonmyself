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

  def main_menu
    render :file => "#{RAILS_ROOT}/app/views/menu_items/_menubar.rhtml", :use_full_path => false, :locals => {:level => 0, :depth => 0, :class_attr => nil, :menu_data => self.menu_data }
  end
  def menu_data 
    return [
      {   
        'text' => 'Home',
        'link' => '/',
        'selected' => 1
      },
      {
        'text' => 'Members',
        'link' => '/members',
        'selected' => 0
      }
    ]
  end
end
