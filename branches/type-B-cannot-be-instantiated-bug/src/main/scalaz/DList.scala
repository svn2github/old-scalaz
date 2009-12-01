package scalaz

sealed trait DList[A] {
  def apply(as: List[A]): List[A]

  def toList = apply(Nil)

  import Scalaz._

  def head = toList.headOption

  def tail = toList match {
    case Nil => None
    case _ :: t => Some(t)
  }

  def ::(a: A) = dlist(a :: apply(_: List[A]))

  def ::>(a: A) = dlist((d: List[A]) => apply(a :: d))

  def :::(as: DList[A]) = dlist((d: List[A]) => apply(as(d)))

  def foldRight[B](b: B, f: (A, B) => B): B = toList.foldRight(b)(f)

  def map[B](f: A => B) = foldRight[DList[B]](emptyDList[B], f(_) :: _)

  def flatMap[B](f: A => DList[B]) = foldRight[DList[B]](emptyDList[B], f(_) ::: _)

  def foreach(f: A => Unit) = toList foreach f

  override def toString = 'D' + toList.toString
}

trait DLists {
  def dlist[A](f: List[A] => List[A]): DList[A] = new DList[A] {
    def apply(as: List[A]) = f(as)
  }

  def emptyDList[A] = dlist(identity(_: List[A]))
}
