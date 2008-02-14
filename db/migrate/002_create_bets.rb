class CreateBets < ActiveRecord::Migration
  def self.up
    create_table :bets do |t|
      t.integer :user_id, :null => false, :references => :users
      t.integer :state, :null => false
      t.string :descr
      t.string :descr_orig
      t.integer :price
      t.string :notes
      t.string :congrats
      t.date :due_date
      t.timestamp :completion_date
      t.timestamps
    end
  end

  def self.down
    drop_table :bets
  end
end
