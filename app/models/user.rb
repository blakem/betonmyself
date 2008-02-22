require 'digest/sha1'
class User < ActiveRecord::Base
  include BomUtility
  has_many :transactions
  has_many :bets
  has_many :surveys
  has_many :feedbacks
  has_many :cash_out_requests

  # Virtual attribute for the unencrypted password
  attr_accessor :password
  attr_accessor :old_password
  attr_accessor :old_password_required

  validates_presence_of     :login, :email, :first_name, :last_name, :role
  validates_presence_of     :password,                   :if => :password_required?
  validates_presence_of     :password_confirmation,      :if => :password_required?
  validates_length_of       :password, :within => 5..40, :if => :password_required?
  validates_confirmation_of :password,                   :if => :password_required?
  validates_length_of       :login,    :within => 4..40
  validates_length_of       :email,    :within => 4..100
  validates_uniqueness_of   :login, :email, :case_sensitive => false
  before_save :encrypt_password
  before_create :make_activation_code 
  # prevents a user from submitting a crafted form that bypasses activation
  # anything else you want your user to change should be added here.
  attr_accessible :login, :email, :password, :password_confirmation, 
    :first_name, :last_name

  # Activates the user in the database.
  def activate
    @activated = true
    self.activated_at = Time.now.utc
    self.activation_code = nil
    save(false)
  end

  def active?
    # the existence of an activation code means they have not activated yet
    activation_code.nil?
  end

  # Returns true if the user has just been activated.
  def pending?
    @activated
  end

  # Authenticates a user by their login name and unencrypted password.  Returns the user or nil.
  def self.authenticate(login, password)
    u = find :first, :conditions => ['login = ? and activated_at IS NOT NULL', login] # need to get the salt
    u && u.authenticated?(password) ? u : nil
  end

  # Encrypts some data with the salt.
  def self.encrypt(password, salt)
    Digest::SHA1.hexdigest("--#{salt}--#{password}--")
  end

  # Encrypts the password with the user salt
  def encrypt(password)
    self.class.encrypt(password, salt)
  end

  def authenticated?(password)
    crypted_password == encrypt(password)
  end

  def remember_token?
    remember_token_expires_at && Time.now.utc < remember_token_expires_at 
  end

  # These create and unset the fields required for remembering users between browser closes
  def remember_me
    remember_me_for 2.weeks
  end

  def remember_me_for(time)
    remember_me_until time.from_now.utc
  end

  def remember_me_until(time)
    self.remember_token_expires_at = time
    self.remember_token            = encrypt("#{email}--#{remember_token_expires_at}")
    save(false)
  end

  def forget_me
    self.remember_token_expires_at = nil
    self.remember_token            = nil
    save(false)
  end

  def balance
    total_funds_in - total_funds_out - total_expired_bet_funds - total_current_bet_funds + total_loans_in
  end
  def available_cashout_funds
    fees = fee_total - total_expired_bet_funds
    fees = 0 if fees < 0
    balance - fees - total_loans_in
  end
  def straight_up_cashout_funds
    balance - fee_total - total_loans_in
  end

  def total_expired_bet_funds
    total = 0
    self.failed_bets.each do |b|
      total += b.price
    end
    total
  end
  def total_current_bet_funds
    total = 0
    self.current_bets.each do |b|
      total += b.price
    end
    total
  end
  def total_accomplishment_funds
    total = 0
    self.accomplishments.each do |b|
      total += b.price
    end
    total
  end
  def total_funds_in
    total = 0
    self.transactions_in.each do |t|
      total += t.price
    end
    total
  end
  def total_funds_out
    total = 0
    self.transactions_out.each do |t|
      total += t.price
    end
    total
  end
  def total_loans_in
    total = 0
    self.loans_in.each do |t|
      total += t.price
    end
    total
  end
  def total_loans_out
    total = 0
    self.loans_out.each do |t|
      total += t.price
    end
    total
  end
  def fee_total
    total = 0
    self.transactions_in.each do |t|
      total += t.fee_amount_calc
    end
    total
  end

  def accomplishments
    accomplishments = []
    self.bets.each do |b|
      if b.state == BomConstant::BET_STATE_SUCCESS
        accomplishments.push b
      end
    end
    return accomplishments
  end
  def accomplishments_deleted
    accomplishments = []
    self.bets.each do |b|
      if b.state == BomConstant::BET_STATE_DELETED
        accomplishments.push b
      end
    end
    return accomplishments
  end

  def current_bets
    current_bets = []
    self.bets.sort {|a,b| a.due_date <=> b.due_date}.each do |b|
      if b.state == BomConstant::BET_STATE_CURRENT
        current_bets.push b
      end
    end
    return current_bets
  end

  def failed_bets
    failed_bets = []
    self.bets.sort {|a,b| a.due_date <=> b.due_date}.each do |b|
      if b.state == BomConstant::BET_STATE_FAILURE
        failed_bets.push b
      end
    end
    return failed_bets
  end

  def transactions_in
    transactions_in = []
    self.transactions_successful.each do |t|
      if t.direction == BomConstant::TRANSACTION_DIRECTION_IN
        transactions_in.push t
      end
    end
    return transactions_in
  end

  def loans
    loans = []
    self.transactions_successful.each do |t|
      if t.trans_type == BomConstant::TRANSACTION_TYPE_BOM_LOAN
        loans.push t
      end
    end
    return loans
  end

  def loans_in
    loans = []
    self.loans.each do |t|
      if t.direction == BomConstant::TRANSACTION_DIRECTION_IN
        loans.push t
      end
    end
    return loans
  end

  def loans_out
    loans = []
    self.loans.each do |t|
      if t.direction == BomConstant::TRANSACTION_DIRECTION_OUT
        loans.push t
      end
    end
    return loans
  end

  def transactions_out
    transactions_out = []
    self.transactions_successful.each do |t|
      if t.direction == BomConstant::TRANSACTION_DIRECTION_OUT
        transactions_out.push t
      end
    end
    return transactions_out
  end

  def transactions_successful
    transactions_succ = []
    self.transactions.sort {|a,b| a.created_at <=> b.created_at}.each do |t|
      if t.state == BomConstant::TRANSACTION_STATE_SUCCESS
        transactions_succ.push t
      end
    end
    return transactions_succ
  end
  def items
    items = []
    items.push transactions_successful
    items.push bets
    accomplishments.each do |a|
      new_a = a.dup
      new_a.is_accomplishment = true
      items.push new_a
    end
    accomplishments_deleted.each do |a|
      new_a = a.dup
      new_a.is_accomplishment = true
      items.push new_a
    end
    items = items.flatten.sort { |a,b| a.account_history_sort_date <=> b.account_history_sort_date }
    return items
  end

  def is_admin
    role == BomConstant::ROLE_TYPE_ADMIN
  end
  def is_user
    role == BomConstant::ROLE_TYPE_USER
  end
  def is_demo
    role == BomConstant::ROLE_TYPE_DEMO
  end

  def last_paypal_acct
    acct = ''
    self.transactions_in.sort {|a,b| a.id <=> b.id}.each do |t| 
      acct = t.payer unless t.payer.blank?
    end
    acct
  end
  
  protected
    # before filter 
    def encrypt_password
      return if password.blank?
      self.salt = Digest::SHA1.hexdigest("--#{Time.now.to_s}--#{login}--") if new_record?
      self.crypted_password = encrypt(password)
    end
      
    def password_required?
      crypted_password.blank? || !password.blank?
    end
    
    def make_activation_code

      self.activation_code = Digest::SHA1.hexdigest( Time.now.to_s.split(//).sort_by {rand}.join )
    end
    def validate_on_update
      if old_password_required and not encrypt(old_password) == crypted_password
        errors.add(:old_password, " does not match current password")
      end
      if ((not new_email.nil?) and (new_email !~ /.+\@.+\..+/))
        errors.add(:email, " is invalid")
      end
    end
    def validate
      if errors.on(:password).nil? and not password.blank?
        password_lw = password.downcase
        if password_lw !~ /[a-z]/i
          errors.add(:password, " must contain at least one alphabet character")
        elsif password_lw !~ /[^a-z]/i
          errors.add(:password, " must contain at least one non-alphabet character")
        elsif password_lw == login.downcase
          errors.add(:password, " can not be the same as your login")
        elsif ((password_lw == first_name.downcase) or (password_lw == last_name.downcase) or (password_lw == first_name.downcase + last_name.downcase) or (password_lw == first_name.downcase + ' ' + last_name.downcase))
          errors.add(:password, " can not be the same as your name")
        end
      end
      if errors.on(:email).nil?
        if email !~ /.+\@.+\..+/
          errors.add(:email, " is invalid")
        end
      end
    end
end
