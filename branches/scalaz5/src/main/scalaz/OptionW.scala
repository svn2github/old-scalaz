package scalaz

sealed trait OptionW[A] {
  val value: Option[A]

  import Scalaz._
  
  sealed trait Fold[X] {
    def none(s: => X): X
  }

  def some[X](s: A => X) = new Fold[X] {
    def none(n: => X) = value match {
      case None => n
      case Some(a) => s(a)
    }
  }

  sealed trait Conditional[X] {
    def |(n: => X): X
  }

  def ?[X](s: => X) = new Conditional[X] {
    def |(n: => X) = value match {
      case None => n
      case Some(_) => s
    }
  }

  def ifNone(n: => Unit) = if(value.isEmpty) n

  def err(message: => String) = value getOrElse (error(message))

  def |(a: => A) = value getOrElse a

  def toNull = value getOrElse null.asInstanceOf[A]

  def unary_~(implicit z: Zero[A]) = value getOrElse z.zero

  def toSuccess[E](e: => E) : Validation[E, A] = value match {
    case Some(a) => Success(a)
    case None => Failure(e)
  }

  def toFailure[B](b: => B) : Validation[A, B] = value match {
    case Some(e) => Failure(e)
    case None => Success(b)
  }

  def fst: FirstOption[A] = value

  def lst: LastOption[A] = value
}

trait Options {
  implicit def OptionTo[A](o: Option[A]): OptionW[A] = new OptionW[A] {
    val value = o
  }

  implicit def OptionFrom[A](o: OptionW[A]): Option[A] = o.value

  def some[A](a: A): Option[A] = Some(a)

  def none[A]: Option[A] = None
}
