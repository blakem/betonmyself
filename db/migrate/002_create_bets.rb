class CreateBets < ActiveRecord::Migration
  def self.up
    create_table :bets do |t|
      t.integer :user_id, :null => false, :references => :users
      t.integer :state, :null => false
      t.string :descr
      t.integer :price
      t.string :notes
      t.date :due_date
      t.date :completion_date
      t.timestamps
    end
  end

  def self.down
    drop_table :bets
  end
end
