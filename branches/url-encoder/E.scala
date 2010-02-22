object E {
  def encode(s: String) =
    s.toList flatMap (c =>
      if(c.isLetter || c.isDigit || Set('!', '*', '\'', '(', ')', ';', ':', '@', '&', '=', '+', '$', ',', '/', '?', '%', '#', '[', ']').contains(c))
      List(c)
    else
      c.toString getBytes "UTF-8" map (b => "%" + (b & 0xFF toHexString)) flatMap (z => z) toList) mkString
}
