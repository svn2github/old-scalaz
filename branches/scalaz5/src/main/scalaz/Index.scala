package scalaz

trait Index[-I[_]] {
  def index[A](a: I[A], i: Int): Option[A]
}

object Index {
  implicit val IterableIndex: Index[Iterable] = new Index[Iterable] {
    def index[A](a: Iterable[A], i: Int) = if(i < 0) None else {
      var n = 0
      var k: Option[A] = None
      val it = a.iterator
      while(it.hasNext && k.isEmpty) {
        val z = it.next
        if(n == i) k = Some(z)
        n = n + 1
      }

      k
    }
  }
}
