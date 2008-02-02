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

  def main_menu(selected_button)
    render :file => "#{RAILS_ROOT}/app/views/menu_items/_menubar.rhtml", :use_full_path => false, :locals => {:level => 0, :depth => 0, :class_attr => nil, :menu_data => menu_data(selected_button) }
  end
  def menu_data(selected_button)
    buttons = {
      'welcome' => {
        'text' => 'Welcome',
        'link' => '/',
        'selected' => 0,
      },
      'members' => {
        'text' => 'Members',
        'link' => '/members',
        'selected' => 0,
      },
      'play' => {
        'text' => 'Play!',
        'link' => '/members',
        'selected' => 0,
      },
      'intro' => {
        'text' => 'Intro',
        'link' => '/intro',
        'selected' => 0,
      },
      'signup' => {
        'text' => 'SignUp',
        'link' => '/signup',
        'selected' => 0,
      },
      'help' => {
        'text' => 'Help',
        'link' => '/help',
        'selected' => 0,
      },
      'support' => {
        'text' => 'Support',
        'link' => '/support',
        'selected' => 0,
      },
      'purchase' => {
        'text' => 'Add Money',
        'link' => '/purchase',
        'selected' => 0,
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
      'faq' => {
        'text' => 'FAQ',
        'link' => '/faq',
        'selected' => 0,
      },
      'contact' => {
        'text' => 'Contact',
        'link' => '/contact',
        'selected' => 0,
      },
    };

    if not selected_button.nil? and buttons[selected_button]
      buttons[selected_button]['selected'] = 1
    end

    if not logged_in?
      display_buttons = [
        buttons['welcome'],
        buttons['signup'],
        buttons['members'],
        buttons['faq'],
        buttons['contact'],
      ]
    else
      display_buttons = [
       buttons['intro'],
       buttons['play'],
       buttons['purchase'],
       buttons['cashout'],
       buttons['help'],
       buttons['support'],
       buttons['faq'],
       buttons['contact'],
       buttons['logout'],
      ]
      if not selected_button.nil? and selected_button == 'welcome'
        display_buttons.unshift buttons['welcome']
      end
    end
    return display_buttons
  end
end
