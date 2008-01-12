module BomUtility
  def money_format (integer)
    return sprintf "%.02f", integer / 100;
  end
end
