// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * A categorical monoid.
 *
 * <p>
 * All monoid instances must satisfy the semigroup law and 2 additional laws:
 * <ol>
 * <li><strong>left identity</strong><br/><code>forall a. append(zero, a) == a</code></li>
 * <li><strong>right identity</strong><br/><code>forall a. append(a, zero) == a</code></li>
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Monoid[A] extends Semigroup[A] with Zero[A]

/**
 * Functions over monoids.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Monoid {
  /**
   * Construct a monoid from the given function and zero. These must satisfy the monoid laws.
   */
  def monoid[A](aa: (A, A) => A, z: A) = new Monoid[A] {
    def append(a1: A, a2: A) = aa(a1, a2)
    val zero = z
  }

  /**
   * A monoid for <code>String</code>.
   */
  implicit val StringMonoid = new Monoid[String] {
    def append(a1: String, a2: String) = Semigroup.StringSemigroup.append(a1, a2)
    val zero = ""
  }

  /**
   * A monoid for <code>scala.Option</code>.
   */
  implicit def OptionMonoid[A] = new Monoid[Option[A]] {
    def append(a1: Option[A], a2: Option[A]) = if(a1.isDefined) a1 else a2
    val zero = None
  }

  /**
   * A monoid for <code>scala.List</code>.
   */
  implicit def ListMonoid[A] = new Monoid[List[A]] {
    def append(a1: List[A], a2: List[A]) = a1 ::: a2
    val zero = Nil
  }

  /**
   * A monoid for <code>scala.Stream</code>.
   */
  implicit def StreamMonoid[A] = new Monoid[Stream[A]] {
    def append(a1: Stream[A], a2: Stream[A]) = a1 append a2
    val zero = Stream.empty
  }

  /**
   * A monoid for <code>scala.Array</code>.
   */
  implicit def ArrayMonoid[A] = new Monoid[Array[A]] {
    def append(a1: Array[A], a2: Array[A]) = a1 ++ a2
    val zero = new Array[A](0)
  }

  /**
   * A monoid for <code>scala.Function1</code>.
   */
  implicit def Function1Monoid[A, B](implicit mb: Monoid[B]) = new Monoid[Function1[A, B]] {
    def append(a1: Function1[A, B], a2: Function1[A, B]) = Semigroup.Function1Semigroup(mb).append(a1, a2)
    val zero = (a: A) => mb.zero
  }

  /**
   * Monoids that use addition and 0.
   */
  object Plus {
    /**
     * A monoid for <code>int</code>.
     */
    implicit def IntMonoid = new Monoid[Int] {
      def append(a1: Int, a2: Int) = a1 + a2
      val zero = 0
    }
  }

  /**
   * Monoids that use multiplication and 1.
   */
  object Multiplication {
    /**
     * A monoid for <code>int</code>.
     */
    implicit def IntMonoid = new Monoid[Int] {
      def append(a1: Int, a2: Int) = a1 * a2
      val zero = 1
    }
  }

  /**
   * Monoids that use disjunction and false.
   */
  object Disjunction {
    implicit def BooleanMonoid = new Monoid[Boolean] {
      def append(a1: Boolean, a2: Boolean) = a1 || a2
      val zero = false
    }
  }

  /**
   * Monoids that use conjunction and true.
   */
  object Conjunction {
    implicit def BooleanMonoid = new Monoid[Boolean] {
      def append(a1: Boolean, a2: Boolean) = a1 && a2
      val zero = true
    }
  }
}
