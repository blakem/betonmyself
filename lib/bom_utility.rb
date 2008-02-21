module BomUtility
  def money_format(integer)
    sprintf("%.02f", integer.to_f / 100);
  end
  def sigil_money(price)
    sign = price < 0 ? '-' : ''
    price = price.abs
    sign + "$" + money_format(price)
  end

  def log(logfile_name, msg)
    logfile = "#{RAILS_ROOT}/log/" + logfile_name + "_" + ENV["RAILS_ENV"] + ".log"
    file = File.new(logfile, "a");
    file.puts DateTime.now.to_s + ": " + msg + "\n";
    file.close
  end
  def log_users(msg)
    log('users', msg)
  end
  def log_bets(msg)
    log('bets', msg)
  end
  def log_transactions(msg)
    log('transactions', msg)
  end
  def log_feedback(msg)
    log('feedback', msg)
  end
  def log_paypal(msg)
    log('paypal', msg)
  end

  def log_msg_for_object(string, obj)
    if obj.respond_to? 'reload' and not obj.id.nil?
      obj.reload
    end
    string + ": " + obj.inspect
  end
  def log_users_obj(string, obj)
    log_users(log_msg_for_object(string, obj))
  end
  def log_bets_obj(string, obj)
    log_bets(log_msg_for_object(string, obj))
  end
  def log_transactions_obj(string, obj)
    log_transactions(log_msg_for_object(string, obj))
  end
  def log_paypal_obj(string, obj)
    log_paypal(log_msg_for_object(string, obj))
  end
  def log_feedback_obj(string, obj)
    log_feedback(log_msg_for_object(string, obj))
  end
  def log_object(obj)
    log_object_msg('Object', obj)
  end
  def log_object_msg(string, obj)
    log('objects', log_msg_for_object(string, obj))
  end

  def log_users_create(user)
    log_users_obj("Created user", user)
    Notifier.deliver_sms_user_create(user)
  end
  def log_users_activate(user)
    log_users_obj("Activated user", user)
    Notifier.deliver_sms_user_activate(user)
  end
  def log_users_update(user)
    log_users_obj("Updated user", user)
  end

  def log_bets_create(bet)
    log_bets_obj("Created bet", bet) 
  end
  def log_bets_complete(bet)
    log_bets_obj("Completed bet", bet) 
  end
  def log_bets_fail(bet)
    log_bets_obj("Failed bet", bet) 
  end
  def log_bets_delete(bet)
    log_bets_obj("Deleted bet", bet) 
  end
  def log_bets_delete_failed(bet)
    log_bets_obj("Deleted failed bet", bet) 
  end
  def log_bets_update(bet)
    log_bets_obj("Updated bet", bet)
  end

  def log_transaction_init(transaction)
    log_transactions_obj("Initialized transaction", transaction)
  end
  def log_transaction_in(transaction)
    log_transactions_obj("Successful transaction in", transaction)
    Notifier.deliver_sms_transaction_in(transaction)
  end
  def log_transaction_loan(transaction)
    log_transactions_obj("Successful transaction loan", transaction)
  end
  def log_transaction_out(transaction)
    log_transactions_obj("Successful transaction out", transaction)
    Notifier.deliver_sms_transaction_out(transaction)
  end
  def log_transaction_fail(transaction, obj, stage)
    log_transactions_obj("Failed transaction", transaction)
    log_transactions_obj("Failed transaction: '" + stage + "'", obj)
    Notifier.deliver_sms_transaction_fail(transaction)
  end

  def log_feedback_create(feedback)
    log_feedback_obj("Created Feedback", feedback)
    Notifier.deliver_sms_feedback_create(feedback)
  end
  def log_testimonial_create(feedback)
    log_feedback_obj("Created Testimonial", feedback)
    Notifier.deliver_sms_testimonial_create(feedback)
  end
  def log_problem_create(feedback)
    log_feedback_obj("Created ReportedProblem", feedback)
    Notifier.deliver_sms_problem_create(feedback)
  end

  def make_random_token(length)
    digest = Digest::MD5.hexdigest(random_string)
    return digest[0..length-1]
  end
  def random_string
    count = 0
    File.open("/usr/share/dict/words") do |file|
      while line = file.gets
        count += 1
      end
    end
    i1 = rand(count)+1
    i2 = rand(count)+1

    count = 0
    w1 = ''
    w2 = ''
    File.open("/usr/share/dict/words") do |file|
      while line = file.gets
        count += 1
        w1 = line.chomp if count == i1
        w2 = line.chomp if count == i2
      end
    end
    return w1 + '-' + w2
  end
  def valid_purchase_amount(price)
    valid = [2000, 5000, 10000, 25000, 50000]
    valid.each do |p|
      return true if price == p
    end
    return false
  end
end
