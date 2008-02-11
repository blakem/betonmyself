class Pseudo < ActiveRecord::Base
  def save
    raise "Cannot save a Pseudo Model"
  end
  def method_missing(symbol, *params)
    send $1 if (symbol.to_s =~ /(.*)_before_type_cast$/)
  end
  def transfer_attributes(params)
    params.each do | name, value |
      self.send("#{name}=", value) if self.respond_to?("#{name}=")
    end
  end
end
