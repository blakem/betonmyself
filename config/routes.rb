ActionController::Routing::Routes.draw do |map|
  map.root :controller => "welcome"
  map.resource :session
  #  map.resources :users

  map.connect '/admin',   :controller => 'admin'

  map.connect '/faq',     :controller => 'faq'
  map.connect '/contact', :controller => 'contact'
  map.connect '/intro',   :controller => 'intro'
  map.connect '/story',   :controller => 'story'
  map.connect '/works',   :controller => 'works'

  map.connect '/signup',  :controller => 'users', :action => 'new'
  map.connect '/signup/create', :controller => 'users', :action => 'create'
  map.connect '/activate/:id', :controller => 'users', :action => 'activate'

  map.connect '/demo', :controller => 'demo'
  map.connect '/demo/get_account_balance', :controller => 'demo', :action => 'get_account_balance'
  map.connect '/demo_expired', :controller => 'demo_expired'
  map.connect '/demo_deleted', :controller => 'demo_deleted'
  map.connect '/demo_purchase', :controller => 'demo_purchase'

  map.connect '/members', :controller => 'members'
  map.connect '/members/get_account_balance', :controller => 'members', :action => 'get_account_balance'

  map.connect '/bet/update_table', :controller => 'bet', :action => 'update_table' # xxx
  map.connect '/bet/edit', :controller => 'bet', :action => 'edit'
  map.connect '/bet/update', :controller => 'bet', :action => 'update'
  map.connect '/bet/show', :controller => 'bet', :action => 'show'
  map.connect '/bet/row', :controller => 'bet', :action => 'row'
  map.connect '/bet/list', :controller => 'bet', :action => 'list'
  map.connect '/bet/create', :controller => 'bet', :action => 'create' # xxx
  map.connect '/bet/complete', :controller => 'bet', :action => 'complete'
  map.connect '/bet/complete_submit', :controller => 'bet', :action => 'complete_submit'
  map.connect '/bet/new', :controller => 'bet', :action => 'new'

  map.connect '/accomplishments/update_table', :controller => 'accomplishments', :action => 'update_table' # xxx
  map.connect '/accomplishments/edit', :controller => 'accomplishments', :action => 'edit'
  map.connect '/accomplishments/update', :controller => 'accomplishments', :action => 'update'
  map.connect '/accomplishments/show', :controller => 'accomplishments', :action => 'show'
  map.connect '/accomplishments/row', :controller => 'accomplishments', :action => 'row'
  map.connect '/accomplishments/list', :controller => 'accomplishments', :action => 'list'
  map.connect '/accomplishments/destroy', :controller => 'accomplishments', :action => 'destroy'
  map.connect '/accomplishments/show_search', :controller => 'accomplishments', :action => 'show_search'

  map.connect '/expired', :controller => 'expired'
  map.connect '/expired_bets/update_table', :controller => 'expired_bets', :action => 'update_table' # xxx
  map.connect '/expired_bets/destroy', :controller => 'expired_bets', :action => 'destroy'
  map.connect '/expired_bets/show', :controller => 'expired_bets', :action => 'show'
  map.connect '/expired_bets/row', :controller => 'expired_bets', :action => 'row'
  map.connect '/expired_bets/list', :controller => 'expired_bets', :action => 'list'

  map.connect '/show_deleted', :controller => 'show_deleted'
  map.connect '/deleted_accomplishments/update_table', :controller => 'deleted_accomplishments', :action => 'update_table' # xxx
  map.connect '/deleted_accomplishments/edit', :controller => 'deleted_accomplishments', :action => 'edit'
  map.connect '/deleted_accomplishments/update', :controller => 'deleted_accomplishments', :action => 'update'
  map.connect '/deleted_accomplishments/show', :controller => 'deleted_accomplishments', :action => 'show'
  map.connect '/deleted_accomplishments/row', :controller => 'deleted_accomplishments', :action => 'row'
  map.connect '/deleted_accomplishments/list', :controller => 'deleted_accomplishments', :action => 'list'
  map.connect '/deleted_accomplishments/destroy', :controller => 'deleted_accomplishments', :action => 'destroy'
  map.connect '/deleted_accomplishments/show_search', :controller => 'deleted_accomplishments', :action => 'show_search'

  map.connect '/logout', :controller => 'sessions', :action => 'destroy'
  map.connect '/login', :controller => 'sessions', :action => 'new'

  map.connect '/help', :controller => 'help'
  map.connect '/support', :controller => 'support'
  map.connect '/update_account', :controller => 'update_account'
  map.connect '/update_account/change', :controller => 'update_account', :action => 'change'
  map.connect '/update_account/change_email/:id', :controller => 'update_account', :action => 'change_email'
  map.connect '/account_history', :controller => 'account_history'
  map.connect '/feedback', :controller => 'feedback'
  map.connect '/feedback/submit', :controller => 'feedback', :action => 'submit'
  map.connect '/feedback/thanks', :controller => 'feedback', :action => 'thanks'
  map.connect '/report_problem', :controller => 'report_problem'
  map.connect '/report_problem/submit', :controller => 'report_problem', :action => 'submit'
  map.connect '/report_problem/thanks', :controller => 'report_problem', :action => 'thanks'
  map.connect '/provide_testimonial', :controller => 'provide_testimonial'
  map.connect '/provide_testimonial/submit', :controller => 'provide_testimonial', :action => 'submit'
  map.connect '/provide_testimonial/thanks', :controller => 'provide_testimonial', :action => 'thanks'

  map.connect '/purchase', :controller => 'purchase'
  map.connect '/purchase/form', :controller => 'purchase', :action => 'form'
  map.connect '/purchase/credit', :controller => 'purchase', :action => 'credit'
  map.connect '/purchase/express', :controller => 'purchase', :action => 'express'
  map.connect '/purchase/express_complete', :controller => 'purchase', :action => 'express_complete'
  map.connect '/purchase/complete', :controller => 'purchase', :action => 'complete'

  map.connect '/cash_out', :controller => 'cash_out'
  map.connect '/cash_out/cash_out', :controller => 'cash_out', :action => 'cash_out'
  map.connect '/cash_out/submit', :controller => 'cash_out', :action => 'submit'

  map.connect '/lost_password', :controller => 'lost_password'
  map.connect '/lost_password/email', :controller => 'lost_password', :action => 'email'
  map.connect '/lost_password/sent', :controller => 'lost_password', :action => 'sent'
  map.connect '/lost_password/error', :controller => 'lost_password', :action => 'error'
  map.connect '/lost_password/reset/:id', :controller => 'lost_password', :action => 'reset'



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

  # See how all your routes lay out with "rake routes"

  # Install the default routes as the lowest priority.
  #  map.connect ':controller/:action/:id'
  #  map.connect ':controller/:action/:id.:format'
end
