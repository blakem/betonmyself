module BomUtility
  def money_format (integer)
    return sprintf("%.02f", integer.to_f / 100);
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

  def log_msg_for_object(string, obj)
    obj.reload
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
  def log_object(obj)
    log('objects', log_msg_for_object('Object', obj))
  end

  def log_users_create(user)
    log_users_obj("Created user", user)
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
  def log_bets_update(bet)
    log_bets_obj("Updated bet", bet)
  end

  def log_transaction_init(transaction)
    log_transactions_obj("Initialized transaction", transaction)
  end
  def log_transaction_in(transaction)
    log_transactions_obj("Successful transaction in", transaction)
  end
  def log_transaction_out(transaction)
    log_transactions_obj("Successful transaction out", transaction)
  end
  def log_transaction_fail(obj)
    log_transactions_obj("Failed transaction", obj)
  end
end
