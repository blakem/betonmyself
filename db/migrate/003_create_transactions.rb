class CreateTransactions < ActiveRecord::Migration
  def self.up
    create_table :transactions do |t|
      t.integer :user_id, :null => false, :references => :users
      t.integer :trans_type, :null => false
      t.integer :price
      t.string  :transaction_id
      t.string  :cvv2_code
      t.string  :avs_code
      t.string  :token
      t.timestamps
    end
  end

  def self.down
    drop_table :transactions
  end
end
