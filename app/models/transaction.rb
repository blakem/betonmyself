class Transaction < ActiveRecord::Base
  include TokenGenerator
  include BomUtility
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

  def fee_amount_calc
    value = 0
    if self.fee_amount
      value = self.fee_amount
    elsif self.direction == BomConstant::TRANSACTION_DIRECTION_IN
      value = ((self.price.to_f * 0.029) + 30).to_i
    end
    return value
  end

  def account_summary_action 
    if direction == BomConstant::TRANSACTION_DIRECTION_IN
      'Deposit'
    else
      'Withdraw'
    end
  end
  def account_summary_date
    created_at.strftime("%m/%d/%Y")
  end
  def account_summary_price
    if direction == BomConstant::TRANSACTION_DIRECTION_IN
      sigil_money(price)
    else
      sigil_money(0-price)
    end
  end
  def account_summary_fee
    if direction == BomConstant::TRANSACTION_DIRECTION_IN
      sigil_money(fee_amount_calc)
    else
      '-'
    end
  end
  def account_summary_balance_effect
    if direction == BomConstant::TRANSACTION_DIRECTION_IN
      price - fee_amount_calc
    else
      0 - price
    end
  end
  def account_summary_goal
    '-'
  end
  def account_summary_sort_date
    created_at
  end
end
