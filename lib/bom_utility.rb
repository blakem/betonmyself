module BomUtility
  def money_format (integer)
    return sprintf("%.02f", integer.to_f / 100);
  end
end
