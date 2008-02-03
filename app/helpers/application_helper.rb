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
        'link' => 'http://www.betonmyself.com/',
        'selected' => 0,
      },
      'signup' => {
        'text' => 'SignUp',
        'link' => 'http://www.betonmyself.com/signup',
        'selected' => 0,
      },
      'members' => {
        'text' => 'Members',
        'link' => 'https://members.betonmyself.com/members',
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
      'demo' => {
        'text' => 'Demo',
        'link' => 'http://www.betonmyself.com/demo',
        'selected' => 0,
      },
      'intro' => {
        'text' => 'Intro',
        'link' => 'https://members.betonmyself.com/intro',
        'selected' => 0,
      },
      'play' => {
        'text' => 'Play!',
        'link' => 'https://members.betonmyself.com/members',
        'selected' => 0,
      },
      'purchase' => {
        'text' => 'Add Money',
        'link' => 'https://members.betonmyself.com/purchase',
        'selected' => 0,
      },
      'cashout' => {
        'text' => 'Cash Out',
        'link' => 'https://members.betonmyself.com/cash_out',
        'selected' => 0,
      },
      'help' => {
        'text' => 'Help',
        'link' => 'https://members.betonmyself.com/help',
        'selected' => 0,
      },
      'support' => {
        'text' => 'Support',
        'link' => 'https://members.betonmyself.com/support',
        'selected' => 0,
      },
      'logout' => {
        'text' => 'Log Out',
        'link' => 'https://members.betonmyself.com/logout',
        'selected' => 0,
      },
    };

    if not selected_button.nil? and buttons[selected_button]
      buttons[selected_button]['selected'] = 1
    end

    if not logged_in? or selected_button == "demo" or self.current_user.is_demo
      display_buttons = [
        buttons['welcome'],
        buttons['signup'],
        buttons['members'],
        buttons['faq'],
        buttons['contact'],
        buttons['demo'],
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
