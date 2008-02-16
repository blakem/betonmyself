# Filters added to this controller apply to all controllers in the application.
# Likewise, all the methods added will be available for all controllers.

class ApplicationController < ActionController::Base
  include AuthenticatedSystem
  include BomConstant
  include BomUtility
  include DebugHelper

  # See ActionController::RequestForgeryProtection for details
  # Uncomment the :secret if you're not using the cookie session store
  protect_from_forgery # :secret => 'd69cbd5f4e9588804e7bab6af44d1ceb'

  ActiveScaffold.set_defaults do |config| 
    config.ignore_columns.add [:created_at, :updated_at, :lock_version]
    config.security.default_permission = false
  end

  before_filter :login_required
  def members_authorized?
    return false if not current_user
    logged_in? and current_user.active? and not current_user.is_demo
  end
end

