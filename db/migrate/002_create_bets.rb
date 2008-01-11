class CreateBets < ActiveRecord::Migration
  def self.up
    create_table :bets do |t|
      t.integer :user_id, :null => false, :references => :users
      t.integer :state, :null => false
      t.string :descr
      t.decimal :price, :precision => 8, :scale => 2
      t.string :notes
      t.string :due_date
      t.timestamps
    end
  end

  def self.down
    drop_table :bets
  end
end
