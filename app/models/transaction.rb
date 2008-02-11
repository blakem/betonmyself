class Transaction < ActiveRecord::Base
  include TokenGenerator
  belongs_to :user
  before_create :set_token

  def to_param
    self.token
  end
  
  def response=(info)
    # CC Response
    self.cvv2_code = info.params['cvv2_code'] if info.params['cvv2_code']
    self.avs_code = info.params['avs_code'] if info.params['avs_code']
    self.gross_amount = (info.params['amount'].to_f * 100).to_i if info.params['amount']
    self.transaction_identifier = info.params['transaction_id'] if info.params['transaction_id']
    # Express Checkout Response
    self.price = info.params['gross_amount'] if info.params['gross_amount']
  end
end
