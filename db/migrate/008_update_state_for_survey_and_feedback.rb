class UpdateStateForSurveyAndFeedback < ActiveRecord::Migration
  def self.up
    add_column :surveys, :state, :integer, :default => 1, :null => false
    add_column :feedbacks, :state, :integer, :default => 1, :null => false
  end

  def self.down
    remove_column :surveys, :state
    remove_column :feedbacks, :state
  end
end
