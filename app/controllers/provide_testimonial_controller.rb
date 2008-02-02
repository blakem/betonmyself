class ProvideTestimonialController < ApplicationController
  def index
    @selected_button = 'support'
    @style = 'border: solid 1px #1F7F00; font: 16px arial'
  end
  def submit
    @selected_button = 'support'
    @feedback = Feedback.new(params[:feedback])
    @feedback.subject = 'Testamonial'
    @feedback.feedback_type = BomConstant::FEEDBACK_TYPE_TESTAMONIAL
    @feedback.user_id = self.current_user.id
    @feedback.save!
    log_testamonial_create(@feedback)
    redirect_to :action => 'thanks'
  rescue ActiveRecord::RecordInvalid
    render :action => 'index'
  end
  def thanks
    @selected_button = 'support'
  end
end
