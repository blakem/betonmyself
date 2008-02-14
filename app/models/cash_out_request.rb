class CashOutRequest < ActiveRecord::Base
  def validate_on_create
    if price.nil? or price.blank?
      errors.add(:price, 'can not be blank')
    else
      if price > self.current_user.available_cashout_funds
        errors.add(:price, 'is more than your available cash out funds')
      end
      if price <= 0
        errors.add(:price, "is too low")
      end
    end
    if method == BomConstant::CASH_OUT_TYPE_PAYPAL
      errors.add(:paypal_account, 'can not be blank when selected') if paypal_account.blank?
    elsif method == BomConstant::CASH_OUT_TYPE_GOOGLE
      errors.add(:google_account, 'can not be blank when selected') if google_account.blank?
    elsif method == BomConstant::CASH_OUT_TYPE_CHECK
      errors.add(:mailing_address, 'can not be blank when selected') if mailing_address.blank?
    elsif method == BomConstant::CASH_OUT_TYPE_OTHER
      errors.add(:other, 'text can not be blank when selected') if other.blank?
    else
      errors.add(:method, 'is invalid')
    end
  end
end
