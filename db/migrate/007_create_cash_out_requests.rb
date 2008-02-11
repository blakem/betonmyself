class CreateCashOutRequests < ActiveRecord::Migration
  def self.up
    create_table :cash_out_requests do |t|
      t.integer :user_id, :null => false, :references => :users
      t.integer :state, :null => false
      t.integer :price, :null => false
      t.integer :method, :null => false
      t.string  :paypal_account
      t.string  :google_account
      t.string  :mailing_address
      t.timestamps
    end
  end

  def self.down
    drop_table :cash_out_requests
  end
end
