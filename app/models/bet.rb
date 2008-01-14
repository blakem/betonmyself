class Bet < ActiveRecord::Base
  include BomUtility
  belongs_to :user
  validates_presence_of :user_id, :state, :descr, :price, :due_date
  def validate_on_create
    if not due_date.nil? and due_date < Date.today
      errors.add(:due_date, "is in the past")
    end
    if not price.nil?
      if price > self.user.ballance
        errors.add(:price, "is more than you have in your incentive bank.")
      end
      if price < BomConstant::MINIMUM_BET
        errors.add(:price, "of $" + money_format(price) + 
                   " is too low. Minimum is $" + 
                   money_format(BomConstant::MINIMUM_BET) + ".")
      end
      if errors.count > 0
        self.price /= 100
      end
    end
  end
end
