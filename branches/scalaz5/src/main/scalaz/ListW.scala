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

  def filterM[M[_]](p: A => M[Boolean])(implicit m: Monad[M]): M[List[A]] = value match {
    case Nil => nil[A] η
    case h :: t => {
      def g = t filterM p
      p(h) ∗ (if(_) g ∘ (h :: _) else g)
    }
  }

  def powerset = filterM(_ => List(true, false))

  def partitionM[M[_]](p: A => M[Boolean])(implicit m: Monad[M]): M[(List[A], List[A])] = value match {
    case Nil => (nil[A], nil[A]) η
    case h :: t => p(h) ∗ (b => (t partitionM p) ∘ { case (x, y) => if(b) (h :: x, y) else (x, h :: y) })
  }

  def spanM[M[_]](p: A => M[Boolean])(implicit m: Monad[M]): M[(List[A], List[A])] = value match {
    case Nil => (nil[A], nil[A]) η
    case h :: t => p(h) ∗ (if(_) (t spanM p) ∘ ((h :: (_: List[A])) <-: _) else (nil[A], value) η) 
  }

  def breakM[M[_]](p: A => M[Boolean])(implicit m: Monad[M]): M[(List[A], List[A])] =
    spanM(p(_) ∘ (! _))

  def groupByM[M[_]](p: (A, A) => M[Boolean])(implicit m: Monad[M]): M[List[List[A]]] = value match {
    case Nil => nil[List[A]] η
    case h :: t => spanM(p(h, _)) ∗ { case (x, y) => (y groupByM p) ∘ ((h :: x) :: _) }
  }

  def mapAccumLeft[B, C](c: C, f: (C, A) => (C, B)): (C, List[B]) = value match {
    case Nil => (c, Nil)
    case h :: t => {
      val (i, j) = f(c, h)
      t.mapAccumLeft(i, f) :-> (j :: _)
    }
  }

  def mapAccumRight[B, C](c: C, f: (C, A) => (C, B)): (C, List[B]) = value match {
    case Nil => (c, Nil)
    case h :: t => {
      val (i, j) = t.mapAccumRight(c, f)
      f(i, h) :-> (_ :: j)
    }
  }
}

trait Lists {
  implicit def ListTo[A](as: List[A]): ListW[A] = new ListW[A] {
    val value = as
  }

  implicit def ListFrom[A](as: ListW[A]): List[A] = as.value

  def nil[A]: List[A] = Nil
}
