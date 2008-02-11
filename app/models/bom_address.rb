class BomAddress < Pseudo
  validates_presence_of :address1, :city, :state, :zip
  attr_accessor :address1, :address2, :city, :state, :zip
end
