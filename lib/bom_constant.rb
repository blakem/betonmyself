module BomConstant
  ROLE_TYPE_USER=1
  ROLE_TYPE_DEMO=2
  ROLE_TYPE_ADMIN=3

  BET_STATE_CURRENT=1
  BET_STATE_SUCCESS=2
  BET_STATE_FAILURE=3
  BET_STATE_DELETED=4
  BET_STATE_FAILED_DELETED=5

  TRANSACTION_TYPE_PAYPAL_EXPRESS=1
  TRANSACTION_TYPE_PAYPAL_CREDIT=2

  TRANSACTION_STATE_INIT=1
  TRANSACTION_STATE_SUCCESS=2
  TRANSACTION_STATE_FAIL=3

  TRANSACTION_DIRECTION_IN=1
  TRANSACTION_DIRECTION_OUT=2

  FEEDBACK_TYPE_FEEDBACK=1
  FEEDBACK_TYPE_PROBLEM=2
  FEEDBACK_TYPE_TESTIMONIAL=3

  INITIAL_BUY_IN=2000
  
  MINIMUM_BET=100
  DEFAULT_BET=500

  RECORDS_PER_PAGE=20
               
  CURRENT_YEAR=Date.today.year.to_s 
  FOOTER_TEXT="Copyright &copy; 2007-" + CURRENT_YEAR + ", BetOnMyself"
  TITLE_TEXT="BetOnMyself"
  HEADER_TEXT="Bet On Myself . com"
  HEADER_NOTE="Helping You Get Things Done"

  SMS_EMAIL="blakemills@tmomail.net"
end
