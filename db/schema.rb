# This file is auto-generated from the current state of the database. Instead of editing this file, 
# please use the migrations feature of ActiveRecord to incrementally modify your database, and
# then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your database schema. If you need
# to create the application database on another system, you should be using db:schema:load, not running
# all the migrations from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended to check this file into your version control system.

ActiveRecord::Schema.define(:version => 3) do

  create_table "bets", :force => true do |t|
    t.integer  "user_id",                                  :null => false
    t.integer  "state",                                    :null => false
    t.string   "descr"
    t.decimal  "price",      :precision => 8, :scale => 2
    t.string   "notes"
    t.string   "due_date"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "transactions", :force => true do |t|
    t.integer  "user_id",                                  :null => false
    t.integer  "trans_type",                               :null => false
    t.decimal  "price",      :precision => 8, :scale => 2
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "users", :force => true do |t|
    t.string   "login"
    t.string   "email"
    t.string   "crypted_password",          :limit => 40
    t.string   "salt",                      :limit => 40
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string   "remember_token"
    t.datetime "remember_token_expires_at"
  end

  add_foreign_key "bets", ["user_id"], "users", ["id"], :name => "bets_user_id_fkey"

  add_foreign_key "transactions", ["user_id"], "users", ["id"], :name => "transactions_user_id_fkey"

end
