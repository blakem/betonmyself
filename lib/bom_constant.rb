module BomConstant
  ROLE_TYPE_USER=1
  ROLE_TYPE_DEMO=2
  ROLE_TYPE_ADMIN=3

  BET_STATE_CURRENT=1
  BET_STATE_SUCCESS=2
  BET_STATE_FAILURE=3
  BET_STATE_DELETED=4

  TRANSACTION_TYPE_PAYPAL_EXPRESS=1
  TRANSACTION_TYPE_PAYPAL_CREDIT=2
  TRANSACTION_TYPE_BOM_LOAN=3
  INITIAL_LOAN_AMOUNT=1000

  TRANSACTION_STATE_INIT=1
  TRANSACTION_STATE_SUCCESS=2
  TRANSACTION_STATE_FAIL=3

  TRANSACTION_DIRECTION_IN=1
  TRANSACTION_DIRECTION_OUT=2

  FEEDBACK_TYPE_FEEDBACK=1
  FEEDBACK_TYPE_PROBLEM=2
  FEEDBACK_TYPE_TESTIMONIAL=3

  CASH_OUT_TYPE_PAYPAL=1
  CASH_OUT_TYPE_GOOGLE=2
  CASH_OUT_TYPE_CHECK=3
  CASH_OUT_TYPE_OTHER=4

  CASH_OUT_STATE_PENDING=1
  CASH_OUT_STATE_COMPLETE=2

  INITIAL_BUY_IN=2000
  
  MINIMUM_BET=100
  DEFAULT_BET=500

  RECORDS_PER_PAGE=20
               
  FOOTER_TEXT='Copyright &copy; 2007-' + Date.today.year.to_s + ', BetOnMyself'
  TITLE_TEXT='BetOnMyself'
  HEADER_TEXT='Bet On Myself . com'
  HEADER_NOTE='Helping You Get Your Stuff Done'

  SMS_TO_EMAIL='blakemills@tmomail.net'
  SMS_FROM_EMAIL='sms@betonmyself.com'
  EMAIL_PREFIX='[BetOnMySelf] '
  SUPPORT_EMAIL='support@betonmyself.com'

  ADMIN_LINK='http://www.betonmyself.com:88/admin'
  if ENV["RAILS_ENV"] == "production"
    WWW_URL='http://www.betonmyself.com'
    WWW_LINK= WWW_URL + '/'
    MEMBERS_URL='https://members.betonmyself.com'
    MEMBERS_LINK = MEMBERS_URL + '/'
  else
    DEV_PORT = 1080.to_s;
    WWW_URL = 'http://www.betonmyself.com:' + DEV_PORT
    WWW_LINK= WWW_URL + '/'
    MEMBERS_URL = 'http://www.betonmyself.com:' + DEV_PORT
    MEMBERS_LINK = MEMBERS_URL + '/members'
  end
end
