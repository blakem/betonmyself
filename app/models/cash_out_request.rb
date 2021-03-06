class CashOutRequest < ActiveRecord::Base
  belongs_to :user
  def validate_on_create
    if price.nil? or price.blank?
      errors.add(:price, 'is required')
    else
      if price > self.current_user.available_cashout_funds
        errors.add(:price, 'is more than your available cash out funds')
      end
      if price <= 0
        errors.add(:price, "is too low")
      end
    end
    if method == BomConstant::CASH_OUT_TYPE_PAYPAL
      errors.add(:paypal_account, 'is required when selected') if paypal_account.blank?
    elsif method == BomConstant::CASH_OUT_TYPE_GOOGLE
      errors.add(:google_account, 'is required when selected') if google_account.blank?
    elsif method == BomConstant::CASH_OUT_TYPE_CHECK
      errors.add(:mailing_address, 'is required when selected') if mailing_address.blank?
    elsif method == BomConstant::CASH_OUT_TYPE_OTHER
      errors.add(:other, 'text is required when selected') if other.blank?
    else
      errors.add(:method, 'is invalid')
    end
  end
end
