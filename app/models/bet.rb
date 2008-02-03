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
        errors.add(:price, "is more than you have in your incentive bank. " +
                           "<a href=\"/purchase\">Add More Money</a>");
      end
      if price < BomConstant::MINIMUM_BET
        errors.add(:price, "of $" + money_format(price) + 
                   " is too low. Minimum is $" + 
                   money_format(BomConstant::MINIMUM_BET) + ".")
      end
    end
  end
  def to_label
    if state == BomConstant::BET_STATE_CURRENT
      "Fields for \"#{descr}\""
    else
      "Notes for \"#{descr}\""
    end
  end
  def Bet.authorize_for_user(id, user)
    obj = find_by_id(id)
    if obj and obj.user_id != user.id
      raise ActiveScaffold::RecordNotAllowed
    end
  end
  def Bet.authorize_for_user_id(user_id, user)
    if user_id != user.id
      raise ActiveScaffold::RecordNotAllowed
    end
  end
end
