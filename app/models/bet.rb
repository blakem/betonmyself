class Bet < ActiveRecord::Base
  belongs_to :user
  validates_presence_of :user_id, :state, :descr, :price, :due_date
  def validate
    if price > self.user.ballance
      errors.add(:price, "is more than your ballance") 
    end
  end
end
