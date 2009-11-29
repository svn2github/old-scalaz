package scalaz

trait Semigroup[S] {
  def append(s1: S, s2: => S): S
}

trait Semigroups {
  def semigroup[S](f: (S, => S) => S) = new Semigroup[S] {
    def append(s1: S, s2: => S) = f(s1, s2)
  }
}

object Semigroup {
  import Scalaz._
  
  implicit def ListSemigroup[A]: Semigroup[List[A]] = semigroup(_ ::: _)

  implicit def StreamSemigroup[A]: Semigroup[Stream[A]] = semigroup(_ append _)

  implicit val StringSemigroup: Semigroup[String] = semigroup(_ + _)  
}
