class CreateFeedbacks < ActiveRecord::Migration
  def self.up
    create_table :feedbacks do |t|
      t.integer :user_id, :null => false, :references => :users
      t.integer :feedback_type, :null => false
      t.string :subject, :null => false
      t.string :feedback, :null => false
      t.string :name
      t.string :location
      t.timestamps
    end
  end

  def self.down
    drop_table :feedbacks
  end
end
