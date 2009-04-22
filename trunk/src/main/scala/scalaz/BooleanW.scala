package scalaz

sealed trait BooleanW {
  val isTrue: Boolean

  /**
   * Negation of Conjunction.
   *
   * <pre>
   * p q  p ~&& q
   * 0 0  1
   * 0 1  1
   * 1 0  1
   * 1 1  0
   * </pre>
   */
  def ~&&(q: => BooleanW) = !isTrue || !q.isTrue

  /**
   * Negation of Disjunction.
   *
   * <pre>
   * p q  p ~|| q
   * 0 0  1
   * 0 1  0
   * 1 0  0
   * 1 1  0
   * </pre>
   */
  def ~||(q: => BooleanW) = !isTrue && !q.isTrue

  /**
   * Conditional.
   *
   * <pre>
   * p q  p ==> q
   * 0 0  1
   * 0 1  1
   * 1 0  0
   * 1 1  1
   * </pre>
   */
  def ==>(q: => BooleanW) = !isTrue || q.isTrue

  /**
   * Inverse Conditional.
   *
   * <pre>
   * p q  p <== q
   * 0 0  1
   * 0 1  0
   * 1 0  1
   * 1 1  1
   * </pre>
   */
  def <==(q: => BooleanW) = isTrue || !q.isTrue

  /**
   * Negational of Conditional.
   *
   * <pre>
   * p q  p ~==> q
   * 0 0  0
   * 0 1  0
   * 1 0  1
   * 1 1  0
   * </pre>
   */
  def ~==>(q: => BooleanW) = isTrue && !q.isTrue

  /**
   * Negation of Inverse Conditional.
   *
   * <pre>
   * p q  p ~<== q
   * 0 0  0
   * 0 1  1
   * 1 0  0
   * 1 1  0
   * </pre>
   */
  def ~<==(q: => BooleanW) = !isTrue && q.isTrue

  /**
   * Executes the given side-effect if this boolean value is <code>true</code>.
   */
  def !(t: => Unit) = if(isTrue) t

  /**
   * Executes the given side-effect if this boolean value is <code>false</code>.
   */
  def unless(f: => Unit) = if(!isTrue) f

  /**
   * Executes the given side-effect if this boolean value is <code>true</code>.
   */
  def when(f: => Unit) = if(isTrue) f

  trait Conditional[X] {
    def |(f: => X): X
  }

  /**
   * Conditional operator that returns the first argument if this is <code>true</code>, the second argument otherwise.
   */
  def ?[X](t: => X) = new Conditional[X] {
    def |(f: => X) = if(isTrue) t else f
  }

  /**
   * Returns the given argument in <code>Some</code> if this is <code>true</code>, <code>None</code> otherwise.
   */
  def option[A](a: => A) = if(isTrue) Some(a) else None

  trait ConditionalEither[A, B] {
    def |(b: => B): Either[A, B]
  }
  
  /**
   * Returns the first argument in <code>Left</code> if this is <code>true</code>, otherwise the second argument in
   * <code>Right</code>.
   */
  def either[A, B](a: => A) = new ConditionalEither[A, B] {
    def |(b: => B) = if(isTrue) Right(b) else Left(a)
  }

  /**
   * Returns the given argument if this is <code>true</code>, otherwise, the zero element for the type of the given
   * argument.
   */
  def ??[A](a: => A)(implicit z: Zero[A]) = if(isTrue) a else z.zero

  def !?[A](a: => A)(implicit z: Zero[A]) = if(!isTrue) a else z.zero

  trait GuardPrevent[M[_]] {
    def apply[A](a: => A)(implicit e: Empty[M], p: Pure[M]): M[A]
  }

  def guard[M[_]] = new GuardPrevent[M] {
    def apply[A](a: => A)(implicit e: Empty[M], p: Pure[M]) = if(isTrue) p pure a else e.empty[A]  
  }

  def prevent[M[_]] = new GuardPrevent[M] {
    def apply[A](a: => A)(implicit e: Empty[M], p: Pure[M]) = if(isTrue) e.empty[A] else p pure a   
  }

  def |&&| = BooleanConjunction.conjunction(isTrue)
}

object BooleanW {
  implicit def BooleanTo(b: Boolean): BooleanW = new BooleanW {
    val isTrue = b
  }

  implicit def BooleanFrom(b: BooleanW) = b.isTrue
}
