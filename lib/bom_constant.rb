module BomConstant
  BET_STATE_CURRENT=1
  BET_STATE_SUCCESS=2
  BET_STATE_FAILURE=3
  BET_STATE_DELETED=4

  TRANSACTION_TYPE_IN=1
  TRANSACTION_TYPE_OUT=2

  INITIAL_BUY_IN=2000
  
  MINIMUM_BET=100
  DEFAULT_BET=500

  RECORDS_PER_PAGE=50000
               
  CURRENT_YEAR=Date.today.year.to_s 
  FOOTER_TEXT="Copyright &copy; 2007-" + CURRENT_YEAR + ", BetOnYourself, Inc."
  TITLE_TEXT="BetOnMyself"
  HEADER_TEXT="Bet On Myself . . ."
  HEADER_NOTE="Helping People Get Things Done"
end
