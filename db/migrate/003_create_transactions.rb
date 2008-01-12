class CreateTransactions < ActiveRecord::Migration
  def self.up
    create_table :transactions do |t|
      t.integer :user_id, :null => false, :references => :users
      t.integer :trans_type, :null => false
      t.integer :price
      t.timestamps
    end
  end

  def self.down
    drop_table :transactions
  end
end
