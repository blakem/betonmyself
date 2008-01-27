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
  def log_users_create(user)
    log_users("Created user: " + 
              @user.id.to_s + ":" + 
              @user.login + ':' + 
              @user.email + ':' +
              @user.first_name + ':' +
              @user.last_name
              );
  end
end
