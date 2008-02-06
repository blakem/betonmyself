class Notifier < ActionMailer::Base
  def summarize_transactions(user)
    # Email header info MUST be added here
    recipients user.email
    from  "support@betonmyself.com"
    subject "Your betonmyself transactions"
    # Email body substitutions go here
    body :user=> user
  end
end
