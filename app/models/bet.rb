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
        purchase_url = self.user.is_demo ? '/demo_purchase' : '/purchase'
        errors.add(:price, 'is more than you have in your incentive bank. ' +
                           '<a href="' + purchase_url + '">Add More Money</a>');
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
  def Bet.authorize_for_user_id(user_id, user)
    if user_id != user.id
      raise ActiveScaffold::RecordNotAllowed
    end
  end
  def authorized_for_create?
    return record_authorized?
  end
  def authorized_for_read?
    return record_authorized?
  end
  def authorized_for_update?
    return record_authorized?
  end
  def authorized_for_destroy?
    return record_authorized?
  end
  def record_authorized?
    return false unless current_user
    return true unless existing_record_check?
    return self.user_id == current_user.id
  end
end
