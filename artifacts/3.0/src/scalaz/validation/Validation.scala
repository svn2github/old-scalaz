// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.validation

/**
 * A wrapper around <code>scala.Either</code> that has renamed identifiers (e.g. <code>Left -> Fail</code>,
 * <code>Right -> Success</code>) for validation. Errors are typically stored on the <code>Left</code> side of the
 * underlying <code>Either</code>. This type also has a different <code>Applicative</code> instance to
 * <code>Either</code> in that errors are accumulated on the left (should one or more be encountered).
 *
 * @see scala.Either
 * @see scalaz.control.Applicative
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Validation[E, A] {
  /**
   * The underlying either value.
   */
  val either: Either[E, A]

  /**
   * Returns <code>true</code> if this validation has failed, otherwise <code>false</code>.
   */
  val isFail = either.isLeft

  /**
   * Returns <code>false</code> if this validation has failed, otherwise <code>true</code>.
   */
  val isSuccess = either.isRight

  /**
   * A <code>String</code> representation for validation. 
   */
  override def toString = either match {
    case Left(a) => "Fail(" + a + ')'
    case Right(b) => "Success(" + b + ')'
  }
}

/**
 * Functions over validation values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Validation {
  /**
   * Wraps a <code>scala.Either</code>.
   */
  implicit def EitherValidation[E, A](e: Either[E, A]): Validation[E, A] = new Validation[E, A] {
    val either = e
  }

  /**
   * Unwraps a <code>scala.Either</code>.
   */
  implicit def ValidationEither[E, A](v: Validation[E, A]): Either[E, A] = v.either

  /**
   * Constructs a validation that has succeeded with the given value.
   */
  def success[E, A](a: A): Validation[E, A] = Right(a)

  /**
   * Constructs a validation that has failed with the given error value.
   */
  def fail[E, A](e: E): Validation[E, A] = Left(e)

  /**
   * Constructs a lifted (unital) validation that has failed with the given error value.
   */
  def failU[U[_], E, A](e: E)(implicit u: control.Unit[U]): Validation[U[E], A] = fail[U[E], A](u.unit(e))

  /**
   * An extractor that matches a failed validation.
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
   * @version $LastChangedRevision$<br>
   *          $LastChangedDate$<br>
   *          $LastChangedBy$
   */
  object Fail {
    /**
     * An extractor that returns the failed value if there is one (fails to match otherwise).
     */
    def unapply[E, A](v: Validation[E, A]): Option[E] = v.either match {
      case Left(e) => Some(e)
      case Right(_) => None
    }
  }

  /**
   * An extractor that matches a succeeded validation.
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
   * @version $LastChangedRevision$<br>
   *          $LastChangedDate$<br>
   *          $LastChangedBy$
   */
  object Success {
    /**
     * An extractor that returns the succeeded value if there is one (fails to match otherwise).
     */
    def unapply[E, A](v: Validation[E, A]): Option[A] = v.either match {
      case Right(a) => Some(a)
      case Left(_) => None
    }
  }
}
