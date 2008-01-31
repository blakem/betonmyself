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
  def log_users_create(user)
    log_users("Created user: " + user.to_s)
  end
  def log_users_update(user)
    log_users("Updated user: " + user.to_s)
  end
  def log_transaction_init(transaction)
    log_users("Initialized transaction: " + transaction.to_s)
  end
  def log_transaction_in(transaction)
    log_users("Successful transaction in: " + transaction.to_s)
  end
  def log_transaction_out(transaction)
    log_users("Successful transaction in: " + transaction.to_s)
  end
  def log_bets_create(bet)
    log_bets("Created bet: " + bet.to_s) 
  end
  def log_bets_complete(bet)
    log_bets("Completed bet: " + bet.to_s) 
  end
  def log_bets_fail(bet)
    log_bets("Failed bet: " + bet.to_s) 
  end
  def log_bets_delete(bet)
    log_bets("Deleted bet: " + bet.to_s) 
  end
  def log_bets_update(bet)
    log_bets("Updated bet: " + bet.to_s)
  end
end
