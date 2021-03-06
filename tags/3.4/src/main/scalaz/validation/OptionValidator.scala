package scalaz.validation

/**
 * Validators using a kleisli structure over <code>scala.Option</code>.
 *
 * @see Kleisli
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait OptionValidator[-A, B] {
  /**
   * The underlying kleisli structure.
   */
  val k: scalaz.control.Kleisli[Option, A, B]

  import control.Kleisli._
  import scalaz.OptionW._
  import scalaz.list.NonEmptyList
  import scalaz.list.NonEmptyList.nel

  /**
   * Lift the underlying kleisli structure from <code>Option</code> to <code>Validation</code> using the given error.
   */
  def ^^[E](e: => E) = k.compose[PartialType[Validation, E]#Apply](_ toSuccess e)

  /**
   * Lift the underlying kleisli structure from <code>Option</code> to <code>Validation</code> using the given error in
   * a non-empty list.
   */
  def ^^^[E](e: => E) = k.compose[PartialType[Validation, NonEmptyList[E]]#Apply](_ toSuccess nel(e))

  /**
   * Lift the underlying kleisli structure from <code>Option</code> to <code>Validation</code> using the given error.
   */
  def ^^-[E, AA <: A](e: AA => E) = kleisli[PartialType[Validation, E]#Apply]((a: AA) =>
    k(a) toSuccess (e(a)))

  /**
   * Lift the underlying kleisli structure from <code>Option</code> to <code>Validation</code> using the given error in
   * a non-empty list.
   */
  def ^^^-[E, AA <: A](e: AA => E) = kleisli[PartialType[Validation, NonEmptyList[E]]#Apply]((a: AA) =>
    k(a) toSuccess (nel(e(a))))
}

/**
 * Functions over validators using a kleisli structure over <code>scala.Option</code>.
 *
 * @see Kleisli
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object OptionValidator {
  implicit def optionValidator[A, B](kk: scalaz.control.Kleisli[Option, A, B]) = new OptionValidator[A, B] {
    val k = kk
  }
}
