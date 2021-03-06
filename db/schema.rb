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

ActiveRecord::Schema.define(:version => 8) do

  create_table "bets", :force => true do |t|
    t.integer  "user_id",         :null => false
    t.integer  "state",           :null => false
    t.string   "descr"
    t.string   "descr_orig"
    t.integer  "price"
    t.string   "notes"
    t.string   "congrats"
    t.date     "due_date"
    t.datetime "completion_date"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "cash_out_requests", :force => true do |t|
    t.integer  "user_id",         :null => false
    t.integer  "state",           :null => false
    t.integer  "price",           :null => false
    t.integer  "method",          :null => false
    t.string   "paypal_account"
    t.string   "google_account"
    t.string   "mailing_address"
    t.string   "other"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "feedbacks", :force => true do |t|
    t.integer  "user_id",                      :null => false
    t.integer  "feedback_type",                :null => false
    t.string   "subject",                      :null => false
    t.string   "feedback",                     :null => false
    t.string   "name"
    t.string   "location"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "state",         :default => 1, :null => false
  end

  create_table "pseudos", :force => true do |t|
    t.string "type"
  end

  create_table "surveys", :force => true do |t|
    t.integer  "user_id",                   :null => false
    t.integer  "version",                   :null => false
    t.string   "q1"
    t.string   "q2"
    t.string   "q3"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "state",      :default => 1, :null => false
  end

  create_table "transactions", :force => true do |t|
    t.integer  "user_id",                :null => false
    t.integer  "direction",              :null => false
    t.integer  "state",                  :null => false
    t.integer  "trans_type"
    t.integer  "price"
    t.string   "cvv2_code"
    t.string   "avs_code"
    t.string   "token"
    t.string   "remote_token"
    t.string   "transaction_identifier"
    t.string   "name"
    t.string   "state_or_province"
    t.string   "payer_country"
    t.string   "address_owner"
    t.string   "postal_code"
    t.string   "payer"
    t.string   "payer_status"
    t.integer  "fee_amount"
    t.integer  "gross_amount"
    t.integer  "tax_amount"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "users", :force => true do |t|
    t.string   "login",                                         :null => false
    t.integer  "role",                                          :null => false
    t.string   "first_name",                                    :null => false
    t.string   "last_name",                                     :null => false
    t.string   "email",                                         :null => false
    t.string   "crypted_password",                :limit => 40
    t.string   "salt",                            :limit => 40
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string   "remember_token"
    t.datetime "remember_token_expires_at"
    t.string   "reset_password_token"
    t.datetime "reset_password_token_expires_at"
    t.string   "activation_code",                 :limit => 40
    t.datetime "activated_at"
    t.string   "new_email"
    t.string   "new_email_activation_code",       :limit => 40
  end

  add_foreign_key "bets", ["user_id"], "users", ["id"], :name => "bets_user_id_fkey"

  add_foreign_key "cash_out_requests", ["user_id"], "users", ["id"], :name => "cash_out_requests_user_id_fkey"

  add_foreign_key "feedbacks", ["user_id"], "users", ["id"], :name => "feedbacks_user_id_fkey"

  add_foreign_key "surveys", ["user_id"], "users", ["id"], :name => "surveys_user_id_fkey"

  add_foreign_key "transactions", ["user_id"], "users", ["id"], :name => "transactions_user_id_fkey"

end
