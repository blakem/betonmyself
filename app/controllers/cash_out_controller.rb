class CashOutController < ApplicationController
  def authorized?
    members_authorized?
  end
  def index
    @accomplishments = self.current_user.accomplishments.sort {|a,b| a.completion_date <=> b.completion_date}
    @selected_button = 'cashout'
  end
  def cash_out
    @selected_button = 'cashout'
    if params['commit'] == "Cancel"
      create_survey
      redirect_to '/'
    else
      @ballance = self.current_user.ballance
      create_transaction_out(@ballance)
      create_survey
    end
  end

  protected
    def create_transaction_out(ballance)
      @transaction = Transaction.new(:user_id => self.current_user.id)
      @transaction.price = @ballance
      @transaction.direction = BomConstant::TRANSACTION_DIRECTION_OUT
      @transaction.state = BomConstant::TRANSACTION_STATE_SUCCESS
      @transaction.save!
      log_transaction_out(@transaction)
    end
    def create_survey
      survey_fields = params[:survey]
      if not survey_fields.nil?
        Survey.new(
          :version => 1,
          :user_id => current_user.id,
          :q1 => survey_fields['q1'],
          :q2 => survey_fields['q2'],
          :q3 => survey_fields['q3']
        ).save!
      end
    end
end
