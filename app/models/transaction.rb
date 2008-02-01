class Transaction < ActiveRecord::Base
  include TokenGenerator
  belongs_to :user
  before_create :set_token

  def to_param
    self.token
  end
  
  def response=(info)
    # CC Response
    %w(cvv2_code avs_code amount transaction_id).each do |f|
      self.send("#{f}=", info.params[f])
    end
    # Express Checkout Response
    self.price = info.params['gross_amount'] if info.params['gross_amount']
  end
end
