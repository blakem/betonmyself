class Transaction < ActiveRecord::Base
  belongs_to :user

  def to_s
    "#<Transaction: " +
    "id:" + id.to_s + "," +
    "user_id:" + user_id.to_s + "," +
    "trans_type:" + trans_type.to_s + "," +
    "price:" + price.to_s +
    " >"
  end
end
