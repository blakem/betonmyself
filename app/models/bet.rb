class Bet < ActiveRecord::Base
  belongs_to :user
  validates_presence_of :user_id, :state, :descr, :price, :due_date
  def validate
    if not price.nil? 
      if price > self.user.ballance
        errors.add(:price, "is more than you have in your incentive bank.")
      end
      if price < 500
        errors.add(:price, "is too low. Minimum is $5.")
      end
      if errors.count > 0
        self.price /= 100
      end
    end
  end
end
