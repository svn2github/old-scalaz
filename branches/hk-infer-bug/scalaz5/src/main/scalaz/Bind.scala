package scalaz

trait Bind[Z[_]] {
  def bind[A, B](a: Z[A], f: A => Z[B]): Z[B]
}

object Bind {
  implicit val IdentityBind: Bind[Identity] = new Bind[Identity] {
    def bind[A, B](a: Identity[A], f: A => Identity[B]) = f(a.value)
  }

  implicit val Function0Bind: Bind[Function0] = new Bind[Function0] {
    def bind[A, B](r: Function0[A], f: A => Function0[B]) = f(r.apply)
  }

  implicit val ListBind: Bind[List] = new Bind[List] {
    def bind[A, B](r: List[A], f: A => List[B]) = r flatMap f
  }

  implicit val OptionBind: Bind[Option] = new Bind[Option] {
    def bind[A, B](r: Option[A], f: A => Option[B]) = r flatMap f
  }
}
