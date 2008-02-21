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
    if ENV["RAILS_ENV"] == "production"
      www = 'http://www.betonmyself.com'
      members = 'https://members.betonmyself.com'
      members_link = members + '/'
    else
      dev_port = 1080.to_s;
      www = 'http://www.betonmyself.com:' + dev_port
      members = 'http://www.betonmyself.com:' + dev_port
      members_link = members + '/members'
    end
    buttons = {
      'welcome' => {
        'text' => 'Welcome',
        'link' => www + '/',
        'selected' => 0,
      },
      'signup' => {
        'text' => 'SignUp',
        'link' => members + '/signup',
        'selected' => 0,
      },
      'story' => {
        'text' => 'My Story',
        'link' => www + '/story',
        'selected' => 0,
      },
      'works' => {
        'text' => 'How it Works',
        'link' => www + '/works',
        'selected' => 0,
      },
      'members' => {
        'text' => 'Members',
        'link' => members_link,
        'selected' => 0,
      },
      'faq' => {
        'text' => 'FAQ',
        'link' => www + '/faq',
        'selected' => 0,
      },
      'contact' => {
        'text' => 'Contact',
        'link' => www + '/contact',
        'selected' => 0,
      },
      'demo' => {
        'text' => 'Demo',
        'link' => www + '/demo',
        'selected' => 0,
      },
      'intro' => {
        'text' => 'Intro',
        'link' => members + '/intro',
        'selected' => 0,
      },
      'play' => {
        'text' => 'Play!',
        'link' => members_link,
        'selected' => 0,
      },
      'purchase' => {
        'text' => 'Add Money',
        'link' => members + '/purchase',
        'selected' => 0,
      },
      'cashout' => {
        'text' => 'Cash Out',
        'link' => members + '/cash_out',
        'selected' => 0,
      },
      'help' => {
        'text' => 'Help',
        'link' => members + '/help',
        'selected' => 0,
      },
      'support' => {
        'text' => 'Support',
        'link' => members + '/support',
        'selected' => 0,
      },
      'member_faq' => {
        'text' => 'FAQ',
        'link' => members + '/faq',
        'selected' => 0,
      },
      'member_contact' => {
        'text' => 'Contact',
        'link' => members + '/contact',
        'selected' => 0,
      },
      'logout' => {
        'text' => 'Log Out',
        'link' => members + '/logout',
        'selected' => 0,
      },
    };

    if not selected_button.nil? 
      if buttons[selected_button]
        buttons[selected_button]['selected'] = 1
      end
      if buttons['member_' + selected_button]
        buttons['member_' + selected_button]['selected'] = 1
      end
    end

    if not logged_in? or selected_button == "demo" or selected_button == "signup" or self.current_user.is_demo
      display_buttons = [
        buttons['welcome'],
        buttons['story'],
        buttons['works'],
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
       buttons['member_faq'],
       buttons['member_contact'],
       buttons['logout'],
      ]
    end
    return display_buttons
  end
end
