class CreateTransactions < ActiveRecord::Migration
  def self.up
    create_table :transactions do |t|
      t.integer :user_id, :null => false, :references => :users
      t.integer :type, :null => false
      t.decimal :price, :precision => 8, :scale => 2
      t.timestamps
    end
  end

  def self.down
    drop_table :transactions
  end
end
