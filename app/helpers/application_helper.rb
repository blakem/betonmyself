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
    buttons = {
      'welcome' => {
        'text' => 'Welcome',
        'link' => '/',
        'selected' => 1
      },
      'members' => {
        'text' => 'Members',
        'link' => '/members',
        'selected' => 0
      },
      'play' => {
        'text' => 'Play!',
        'link' => '/members',
        'selected' => 0
      },
      'signup' => {
        'text' => 'SignUp',
        'link' => '/signup',
        'selected' => 0
      },
      'help' => {
        'text' => 'Help',
        'link' => '/help',
        'selected' => 0
      },
      'support' => {
        'text' => 'Support',
        'link' => '/support',
        'selected' => 0
      },
      'purchase' => {
        'text' => 'Add Money',
        'link' => '/purchase',
        'selected' => 0
      },
      'donate' => {
        'text' => 'Donate',
        'link' => '/',
        'selected' => 0
      },
      'logout' => {
        'text' => 'Log Out',
        'link' => '/logout',
        'selected' => 0,
      },
      'cashout' => {
        'text' => 'Cash Out',
        'link' => '/cash_out',
        'selected' => 0,
      },
    };
    if not logged_in?
      [
       buttons['welcome'],
       buttons['signup'],
       buttons['members'],
      ]
    else
      [
       buttons['play'],
       buttons['purchase'],
       buttons['cashout'],
       buttons['help'],
       buttons['support'],
       buttons['donate'],
       buttons['logout'],
      ]
    end
  end
end
