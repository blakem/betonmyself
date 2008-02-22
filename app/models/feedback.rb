class Feedback < ActiveRecord::Base
  belongs_to :user
  validates_presence_of :feedback_type, :subject, :user_id
  validates_presence_of :feedback,    :if => :feedback_required?
  validates_presence_of :testimonial, :if => :testimonial_required?
  validates_presence_of :problem,     :if => :problem_required?
  attr_accessor :testimonial
  attr_accessor :problem

  protected
    def feedback_required?
      feedback_type == BomConstant::FEEDBACK_TYPE_FEEDBACK
    end
    def testimonial_required?
      feedback_type == BomConstant::FEEDBACK_TYPE_TESTIMONIAL
    end
    def problem_required?
      feedback_type == BomConstant::FEEDBACK_TYPE_PROBLEM
    end
end
