package scalaz

trait Length[-L[_]] {
  def len[A](a: L[A]): Int
}

object Length {
  implicit val IterableLength: Length[Iterable] = new Length[Iterable] {
    def len[A](a: Iterable[A]) = {
      var n = 0
      val i = a.iterator
      while(i.hasNext) {
        n = n + 1
        i.next
      }

      n
    }
  }
}
