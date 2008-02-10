class Notifier < ActionMailer::Base
  include BomConstant
  include BomUtility

  def change_email_notification(user)
    from BomConstant::SUPPORT_EMAIL
    recipients user.new_email
    subject BomConstant::EMAIL_PREFIX + 'Please verify your new email'
    body :user => user, :url => "https://members.betonmyself.com/update_account/change_email/#{user.new_email_activation_code}"
  end
  def change_email_notification_old_email(user)
    setup_email(user)
    subject BomConstant::EMAIL_PREFIX + 'Email Change Request'
    body :user => user
  end
  def signup_notification(user)
    setup_email(user)
    subject BomConstant::EMAIL_PREFIX + 'Please activate your new account'
    body :user => user, :url => "https://members.betonmyself.com/activate/#{user.activation_code}"
  end
  def activation(user)
    setup_email(user)
    subject BomConstant::EMAIL_PREFIX + 'Your account has been activated!'
    body :user => user, :url  => "https://members.betonmyself.com/"
  end
  def summarize_transactions(user)
    setup_email(user)
    subject BomConstant::EMAIL_PREFIX + 'Your account transactions'
    body :user => user
  end
  def reset_password(user)
    setup_email(user)
    subject BomConstant::EMAIL_PREFIX + 'How to reset your password'
    body :user => user
  end

  def sms_user_create(user)
    setup_sms
    subject "NEW: " + user.first_name + ' ' + user.last_name + " (" + user.login + ")"
    body :user => user
  end
  def sms_transaction_in(transaction)
    setup_sms
    user = User.find_by_id(transaction.user_id);
    subject "IN: +$" + money_format(transaction.price) + " " + user.first_name + " " + user.last_name + " (" + user.login + ")"
    body :user => user
  end
  def sms_transaction_out(transaction)
    setup_sms
    user = User.find_by_id(transaction.user_id);
    subject "OUT: -$" + money_format(transaction.price) + " " + user.first_name + " " + user.last_name + " (" + user.login + ")"
    body :user => user
  end
  def sms_transaction_fail(transaction)
    setup_sms
    user = User.find_by_id(transaction.user_id);
    subject "TRANSFAIL: $" + money_format(transaction.price) + " " + user.first_name + " " + user.last_name + " (" + user.login + ")"
    body :user => user
  end

  def sms_feedback_create(feedback)
    setup_sms
    user = User.find_by_id(feedback.user_id);
    subject "FEEDBK: " + user.first_name + " " + user.last_name + " (" + user.login + ") " + feedback.subject
    body :user => user, :feedback => feedback
  end
  def sms_problem_create(feedback)
    setup_sms
    user = User.find_by_id(feedback.user_id);
    subject "PROBLEM: " + user.first_name + " " + user.last_name + " (" + user.login + ") " + feedback.subject
    body :user => user, :feedback => feedback
  end
  def sms_testimonial_create(feedback)
    setup_sms
    user = User.find_by_id(feedback.user_id);
    subject "TESTIMONIAL: " + user.first_name + " " + user.last_name + " (" + user.login + ") " + feedback.feedback
    body :user => user, :feedback => feedback
  end
  protected
    def setup_email(user)
      recipients user.email
      from BomConstant::SUPPORT_EMAIL
    end
    def setup_sms
      recipients BomConstant::SMS_TO_EMAIL
      from  BomConstant::SMS_FROM_EMAIL
    end
end
