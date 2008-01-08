class CreateBets < ActiveRecord::Migration
  def self.up
    create_table :bets do |t|
      t.string :descr
      t.string :amount
      t.string :notes
      t.string :due_date
      t.timestamps
    end
  end

  def self.down
    drop_table :bets
  end
end
