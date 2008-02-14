class FeedbackController < ApplicationController
  def authorized?
    members_authorized?
  end
  def index
    @selected_button = 'support'
    @style = 'border: solid 1px #1F7F00; font: 16px arial'
    @feedback = Feedback.new
  end
  def submit
    @selected_button = 'support'
    @feedback = Feedback.new(params[:feedback])
    @feedback.feedback_type = BomConstant::FEEDBACK_TYPE_FEEDBACK
    @feedback.user_id = self.current_user.id
    @feedback.save!
    log_feedback_create(@feedback)
    redirect_to :action => 'thanks'
  rescue ActiveRecord::RecordInvalid
    render :action => 'index'
  end
  def thanks
    @selected_button = 'support'
  end
end
