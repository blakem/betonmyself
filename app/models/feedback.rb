class Feedback < ActiveRecord::Base
  validates_presence_of :subject, :feedback
end
