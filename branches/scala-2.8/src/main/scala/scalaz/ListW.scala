package scalaz

sealed trait ListW[A] {
  val value: List[A]

  def string(f: A => Char) = value map f mkString

  def stringj(f: A => List[Char]) = value flatMap f mkString

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
    case h :: t => Some(NonEmptyList.nel(h, t))
  }

  def <^>[B](f: NonEmptyList[A] => B)(implicit z: Zero[B]) = value match {
    case Nil => z.zero
    case h :: t => f(NonEmptyList.nel(h, t))
  }

  // def dlist[A](as: List[A]): DList[A] = dlist(as ::: _)

  def dlist = DList.dlist(value ::: (_: List[A]))

  def break(p: A => Boolean): (List[A], List[A]) = value.span(!p(_))
}

object ListW {
  implicit def ListTo[A](as: List[A]): ListW[A] = new ListW[A] {
    val value = as
  }

  implicit def ListFrom[A](as: ListW[A]): List[A] = as.value
}
