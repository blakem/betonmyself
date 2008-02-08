class Notifier < ActionMailer::Base
  include BomConstant
  include BomUtility
  def summarize_transactions(user)
    recipients user.email
    from  "support@betonmyself.com"
    subject "Your betonmyself transactions"
    body :user => user
  end

  def sms_user_create(user)
    recipients BomConstant::SMS_EMAIL
    from  "sms@betonmyself.com"
    subject "NEW: " + user.first_name + ' ' + user.last_name + " (" + user.login + ")"
    body :user => user
  end
  def sms_transaction_in(transaction)
    user = User.find_by_id(transaction.user_id);
    recipients BomConstant::SMS_EMAIL
    from  "sms@betonmyself.com"
    subject "IN: +$" + money_format(transaction.price) + " " + user.first_name + " " + user.last_name + " (" + user.login + ")"
    body :user => user
  end
  def sms_transaction_out(transaction)
    user = User.find_by_id(transaction.user_id);
    recipients BomConstant::SMS_EMAIL
    from  "sms@betonmyself.com"
    subject "OUT: -$" + money_format(transaction.price) + " " + user.first_name + " " + user.last_name + " (" + user.login + ")"
    body :user => user
  end
  def sms_transaction_fail(transaction)
    user = User.find_by_id(transaction.user_id);
    recipients BomConstant::SMS_EMAIL
    from  "sms@betonmyself.com"
    subject "TRANSFAIL: $" + money_format(transaction.price) + " " + user.first_name + " " + user.last_name + " (" + user.login + ")"
    body :user => user
  end

  def sms_feedback_create(feedback)
    user = User.find_by_id(feedback.user_id);
    recipients BomConstant::SMS_EMAIL
    from  "sms@betonmyself.com"
    subject "FEEDBK: " + user.first_name + " " + user.last_name + " (" + user.login + ") " + feedback.subject
    body :user => user, :feedback => feedback
  end
  def sms_problem_create(feedback)
    user = User.find_by_id(feedback.user_id);
    recipients BomConstant::SMS_EMAIL
    from  "sms@betonmyself.com"
    subject "PROBLEM: " + user.first_name + " " + user.last_name + " (" + user.login + ") " + feedback.subject
    body :user => user, :feedback => feedback
  end
  def sms_testimonial_create(feedback)
    user = User.find_by_id(feedback.user_id);
    recipients BomConstant::SMS_EMAIL
    from  "sms@betonmyself.com"
    subject "TESTIMONIAL: " + user.first_name + " " + user.last_name + " (" + user.login + ") " + feedback.feedback
    body :user => user, :feedback => feedback
  end
end
