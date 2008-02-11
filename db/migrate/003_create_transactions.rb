class CreateTransactions < ActiveRecord::Migration
  def self.up
    create_table :transactions do |t|
      t.integer :user_id, :null => false, :references => :users
      t.integer :direction, :null => false
      t.integer :state, :null => false
      t.integer :trans_type
      t.integer :price
      t.string  :cvv2_code
      t.string  :avs_code
      t.string  :token
      t.string  :remote_token
      t.string  :name
      t.string  :state_or_province
      t.string  :payer_country
      t.string  :address_owner
      t.string  :postal_code
      t.string  :payer
      t.string  :payer_status
      t.integer :fee_amount
      t.integer :gross_amount
      t.integer :tax_amount
      t.timestamps
    end
  end

  def self.down
    drop_table :transactions
  end
end
