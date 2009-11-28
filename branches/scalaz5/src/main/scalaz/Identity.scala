package scalaz

sealed trait Identity[A] {
  val value: A

  def ⊹(a: => A)(implicit s: Semigroup[A]) = s append (value, a)

  def ≟(a: A)(implicit e: Equal[A]) = e equal (value, a)

  def ≠(a: A)(implicit e: Equal[A]) = !(≟(a))

  def ?|?(a: A)(implicit o: Order[A]) = o order (value, a)

  def ≤(a: A)(implicit o: Order[A]) = o.order(value, a) != GT

  def ≥(a: A)(implicit o: Order[A]) = o.order(value, a) != LT

  def ≨(a: A)(implicit o: Order[A]) = o.order(value, a) == LT

  def ≩(a: A)(implicit o: Order[A]) = o.order(value, a) == GT

  def ≮(a: A)(implicit o: Order[A]) = o.order(value, a) != LT

  def ≯(a: A)(implicit o: Order[A]) = o.order(value, a) != GT

  def ≰(a: A)(implicit o: Order[A]) = o.order(value, a) == GT

  def ≱(a: A)(implicit o: Order[A]) = o.order(value, a) == LT

  def show(implicit s: Show[A]) = s.show(value)

  def shows(implicit s: Show[A]) = s.shows(value)

  def print(implicit s: Show[A]) = Console.print(shows)

  def println(implicit s: Show[A]) = Console.println(shows)

  def text(implicit s: Show[A]) = xml.Text(s shows value)
}

trait Identitys {
  implicit def IdentityTo[A](x: A): Identity[A] = new Identity[A] {
    val value = x
  }

  implicit def IdentityFrom[A](x: Identity[A]): A = x.value

  val unital = IdentityTo(())
}
