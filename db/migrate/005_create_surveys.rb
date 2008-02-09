class CreateSurveys < ActiveRecord::Migration
  def self.up
    create_table :surveys do |t|
      t.integer :user_id, :null => false, :references => :users
      t.integer :version, :null => false
      t.string :q1
      t.string :q2
      t.string :q3
      t.timestamps
    end
  end

  def self.down
    drop_table :surveys
  end
end
