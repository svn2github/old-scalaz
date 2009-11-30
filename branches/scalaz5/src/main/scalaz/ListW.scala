package scalaz

sealed trait ListW[A] {
  val value: List[A]

  import Scalaz._

  def intersperse(a: A): List[A] = value match {
    case Nil => Nil
    case x :: Nil => x :: Nil
    case h :: t => h :: a :: t.intersperse(a)
  }

  def intercalate(as: List[A]): List[A] = value match {
    case Nil => Nil
    case x :: Nil => x :: Nil
    case h :: t => h :: as ::: t.intercalate(as)
  }

  def nel = value match {
    case Nil => None
    case h :: t => Some(Scalaz.nel(h, t))
  }

  def <^>[B](f: NonEmptyList[A] => B)(implicit z: Zero[B]) = value match {
    case Nil => z.zero
    case h :: t => f(Scalaz.nel(h, t))
  }

  def stripPrefix(prefix : List[A]) : Option[List[A]] = {
    val (before, after) = value splitAt prefix.length
    (before == prefix) option after
  }

  def dlist: DList[A] = Scalaz.dlist(value ::: (_: List[A]))

  def takeWhileM[M[_]](p: A => M[Boolean])(implicit m: Monad[M]): M[List[A]] = value match {
    case Nil => nil[A] η
    case h :: t => p(h) ∗ (if(_) (t takeWhileM p) ∘ (h :: _) else nil[A] η)
  }
}

trait Lists {
  implicit def ListTo[A](as: List[A]): ListW[A] = new ListW[A] {
    val value = as
  }

  implicit def ListFrom[A](as: ListW[A]): List[A] = as.value

  def nil[A]: List[A] = Nil
}
