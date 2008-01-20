ActionController::Routing::Routes.draw do |map|

  map.resource :session
  map.resources :users


  map.faq '/faq', :controller => 'faq'
  map.contact '/contact', :controller => 'contact'
  map.lost_password '/lost_password', :controller => 'lost_password'
  map.donate '/donate', :controller => 'donate'
  map.help '/help', :controller => 'help'
  map.purchase '/purchase', :controller => 'purchase'
  map.cash_out '/cash_out', :controller => 'cash_out'
  map.signup '/signup', :controller => 'users', :action => 'new'
  map.login  '/login', :controller => 'sessions', :action => 'new'
  map.logout '/logout', :controller => 'sessions', :action => 'destroy'

  map.support '/support', :controller => 'support'
  map.update_account '/update_account', :controller => 'update_account'
  map.provide_testimonial '/provide_testimonial', :controller => 'provide_testimonial'
  map.feedback '/feedback', :controller => 'feedback'
  map.report_problem '/report_problem', :controller => 'report_problem'
  map.summarize_transactions '/summarize_transactions', :controller => 'summarize_transactions'

  # The priority is based upon order of creation: first created -> highest priority.

  # Sample of regular route:
  #   map.connect 'products/:id', :controller => 'catalog', :action => 'view'
  # Keep in mind you can assign values other than :controller and :action

  # Sample of named route:
  #   map.purchase 'products/:id/purchase', :controller => 'catalog', :action => 'purchase'
  # This route can be invoked with purchase_url(:id => product.id)

  # Sample resource route (maps HTTP verbs to controller actions automatically):
  #   map.resources :products

  # Sample resource route with options:
  #   map.resources :products, :member => { :short => :get, :toggle => :post }, :collection => { :sold => :get }

  # Sample resource route with sub-resources:
  #   map.resources :products, :has_many => [ :comments, :sales ], :has_one => :seller

  # Sample resource route within a namespace:
  #   map.namespace :admin do |admin|
  #     # Directs /admin/products/* to Admin::ProductsController (app/controllers/admin/products_controller.rb)
  #     admin.resources :products
  #   end

  # You can have the root of your site routed with map.root -- just remember to delete public/index.html.
  map.root :controller => "welcome"

  # See how all your routes lay out with "rake routes"

  # Install the default routes as the lowest priority.
  map.connect ':controller/:action/:id'
  map.connect ':controller/:action/:id.:format'
end
