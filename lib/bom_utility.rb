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
    log_users("Created user: " + 
              user.id.to_s + ":" + 
              user.login + ':' + 
              user.email + ':' +
              user.first_name + ':' +
              user.last_name
              );
  end
  def log_transaction(transaction)
    log_users("Created transaction: " +
              transaction.id.to_s + ":" +
              transaction.user_id.to_s + ":" +
              transaction.trans_type.to_s + ":" +
              transaction.price.to_s
              );
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
    log_bets("Failed bet: " + bet.to_s) 
  end
end
