class ProvideTestimonialController < ApplicationController
  def authorized?
    logged_in? and not current_user.is_demo
  end
  def index
    @selected_button = 'support'
    @style = 'border: solid 1px #1F7F00; font: 16px arial'
  end
  def submit
    @selected_button = 'support'
    @feedback = Feedback.new(params[:feedback])
    @feedback.subject = 'Testimonial'
    @feedback.feedback_type = BomConstant::FEEDBACK_TYPE_TESTAMONIAL
    @feedback.user_id = self.current_user.id
    @feedback.save!
    log_testimonial_create(@feedback)
    redirect_to :action => 'thanks'
  rescue ActiveRecord::RecordInvalid
    render :action => 'index'
  end
  def thanks
    @selected_button = 'support'
  end
end
