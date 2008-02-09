class CreateUsers < ActiveRecord::Migration
  def self.up
    create_table "users", :force => true do |t|
      t.string :login, :null => false
      t.integer :role, :null => false
      t.string :first_name, :null => false
      t.string :last_name, :null => false
      t.string :email, :null => false
      t.string :crypted_password, :limit => 40
      t.string :salt, :limit => 40
      t.datetime :created_at
      t.datetime :updated_at
      t.string :remember_token
      t.datetime :remember_token_expires_at
      t.string :reset_password_token
      t.datetime :reset_password_token_expires_at
      t.string :activation_code, :limit => 40
      t.datetime :activated_at
    end
  end

  def self.down
    drop_table "users"
  end
end
