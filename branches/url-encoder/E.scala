object E {
  def encode(s: String) =
    s.toList flatMap (c =>
      if(c.isLetter || c.isDigit || Set('!', '*', '\'', '(', ')', ';', ':', '@', '&', '=', '+', '$', ',', '/', '?', '%', '#', '[', ']').contains(c))
      List(c)
    else
      c.toString getBytes "UTF-8"
        flatMap (b => "%" + (b & 0xFF toHexString)) toList) mkString
}
