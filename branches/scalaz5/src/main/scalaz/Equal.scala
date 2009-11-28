package scalaz

trait Equal[-A] {
  def equal(a1: A, a2: A): Boolean
}

object Equal {
  def equal[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A) = f(a1, a2)
  }

  import Scalaz._
    
  def equalA[A] = equal[A](_ == _)

  implicit val CharEqual: Equal[Char] = equalA

  implicit def IterableEqual[A](implicit ea: Equal[A]) = equal[Iterable[A]]((a1, a2) => {
    val i1 = a1.iterator
    val i2 = a2.iterator
    var b = false

    while (i1.hasNext && i2.hasNext && !b) {
      val x1 = i1.next
      val x2 = i2.next

      if (x1 ≠ x2) {
        b = true
      }
    }

    !(b || i1.hasNext || i2.hasNext)
  })
}
