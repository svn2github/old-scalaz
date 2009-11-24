package scalaz

object Scalaz extends ScalazLowPriorityImplicits {
  import java.math.BigInteger

  implicit def IdentityTo[A](x: A) = Identity.IdentityTo(x)

  implicit def AlphaChar(a: Alpha) = Alpha.AlphaChar(a)

  implicit def ArrayByteTo(bs: Array[Byte]) = ArrayByte.ArrayByteTo(bs)

  implicit def ArrayByteFrom(bs: ArrayByte) = ArrayByte.ArrayByteFrom(bs)

  implicit def BigIntTo(n: BigInt) = BigIntW.BigIntTo(n)

  implicit def BigIntFrom(n: BigIntW) = BigIntW.BigIntFrom(n)

  implicit def BigIntegerTo(n: BigInteger) = BigIntegerW.BigIntegerTo(n)

  implicit def BigIntegerFrom(n: BigIntegerW) = BigIntegerW.BigIntegerFrom(n)

  implicit def BooleanTo(b: Boolean) = BooleanW.BooleanTo(b)

  implicit def BooleanFrom(b: BooleanW) = BooleanW.BooleanFrom(b)

  def conjunction(b: Boolean) = BooleanConjunction.conjunction(b)

  implicit def BooleanConjunctionFrom(b: BooleanConjunction) = BooleanConjunction.BooleanConjunctionFrom(b)

  def multiplication(n: Byte) = ByteMultiplication.multiplication(n)

  implicit def ByteMultiplicationFrom(n: ByteMultiplication) = ByteMultiplication.ByteMultiplicationFrom(n)

  implicit def ByteTo(n: Byte) = ByteW.ByteTo(n)

  implicit def ByteFrom(n: ByteW) = ByteW.ByteFrom(n)

  def multiplication(n: Char) = CharMultiplication.multiplication(n)

  implicit def CharMultiplicationFrom(n: CharMultiplication) = CharMultiplication.CharMultiplicationFrom(n)

  implicit def CharTo(c: Char) = CharW.CharTo(c)

  implicit def CharFrom(c: CharW) = CharW.CharFrom(c)

  def cokleisli[W[_]] = Cokleisli.cokleisli[W]

  def continuation[R, A](f: (A => R) => R) = Continuation.continuation(f)

  implicit def DigitLong(d: Digit) = Digit.DigitLong(d)

  implicit def LongDigit(n: Long) = Digit.LongDigit(n)

  implicit def DoubleTo(n: Double) = DoubleW.DoubleTo(n)

  implicit def DoubleFrom(n: DoubleW) = DoubleW.DoubleFrom(n)

  def multiplication(n: Long) = LongMultiplication.multiplication(n)

  implicit def EndoTo[A](f: A => A) = Endo.EndoTo(f)

  implicit def EndoFrom[A](e: Endo[A]) = Endo.EndoFrom(e)

  implicit def EnumerationTo[A](v: java.util.Enumeration[A]) = EnumerationW.EnumerationTo(v)

  implicit def EnumerationFrom[A](v: EnumerationW[A]) = EnumerationW.EnumerationFrom(v)

  implicit def LongMultiplicationFrom(n: LongMultiplication) = LongMultiplication.LongMultiplicationFrom(n)

  implicit def Function0To[T](f: () => T) = Function0W.Function0To(f)

  implicit def Function0From[T](f: Function0W[T]) = Function0W.Function0From(f)

  implicit def Function1To[T, R](f: T => R) = Function1W.Function1To(f)

  implicit def Function1From[T, R](f: Function1W[T, R]) = Function1W.Function1From(f)

  implicit def Function2To[T1, T2, R](f: (T1, T2) => R) = Function2W.Function2To(f)

  implicit def Function2From[T1, T2, R](f: Function2W[T1, T2, R]) = Function2W.Function2From(f)

  def multiplication(n: Int) = IntMultiplication.multiplication(n)

  implicit def InputStreamTo(v: java.io.InputStream) = InputStreamW.InputStreamTo(v)

  implicit def InputStreamFrom(v: InputStreamW) = InputStreamW.InputStreamFrom(v)

  implicit def IntMultiplicationFrom(n: IntMultiplication) = IntMultiplication.IntMultiplicationFrom(n)

  implicit def IntTo(n: Int) = IntW.IntTo(n)

  implicit def IntFrom(n: IntW) = IntW.IntFrom(n)

  implicit def IterableTo[A](i: Iterable[A]) = IterableW.IterableTo(i)

  implicit def IterableFrom[A](i: IterableW[A]) = IterableW.IterableFrom(i)

  implicit def JavaIterableTo[A](i: java.lang.Iterable[A]): IterableW[A] = IterableW.JavaIterableTo(i)

  def kleisli[M[_]] = Kleisli.kleisli[M]

  implicit def ListTo[A](as: List[A]): ListW[A] = ListW.ListTo(as)

  implicit def ListFrom[A](as: ListW[A]) = ListW.ListFrom(as)

  implicit def LongTo(n: Long) = LongW.LongTo(n)

  implicit def LongFrom(n: LongW) = LongW.LongFrom(n)

  implicit def OptionTo[A](o: Option[A]) = OptionW.OptionTo(o)

  implicit def OptionFrom[A](o: OptionW[A]) = OptionW.OptionFrom(o)

  def multiplication(n: Short) = ShortMultiplication.multiplication(n)

  implicit def ShortMultiplicationFrom(n: ShortMultiplication) = ShortMultiplication.ShortMultiplicationFrom(n)

  implicit def ShortTo(n: Short) = ShortW.ShortTo(n)

  implicit def ShortFrom(n: ShortW) = ShortW.ShortFrom(n)

  implicit def StreamTo[A](as: Stream[A]) = StreamW.StreamTo(as)

  implicit def StreamFrom[A](as: StreamW[A]) = StreamW.StreamFrom(as)

  implicit def StringTo(ss: String) = StringW.StringTo(ss)

  implicit def StringFrom(s: StringW) = StringW.StringFrom(s)

  def zipStream[A](s: Stream[A]) = ZipStream.zip(s)

  implicit def ZipStreamFrom[A](z: ZipStream[A]) = ZipStream.ZipStreamFrom(z)

  def zipper[A](ls: Stream[A], a: A, rs: Stream[A]) = Zipper.zipper(ls, a, rs)

  import concurrent._
  implicit def StrategyTo[A](s: Strategy[A]) = Strategy.strategyTo(s)

  implicit def StrategyFrom[A](s: (() => A) => () => A) = Strategy.strategyFrom(s)

  // MA
  import MA._

  implicit def ContinuationMA[R, A](a: Continuation[R, A]): MA[PartialApply1Of2[Continuation, R]#Apply, A] = maPartial[PartialApply1Of2[Continuation, R]#Apply](a)

  implicit def StateMA[S, A](a: State[S, A]): MA[PartialApply1Of2[State, S]#Apply, A] = ma[PartialApply1Of2[State, S]#Apply, A](a)

  implicit def ComemoMA[T, K, V](a: memo.Comemo[T, K, V]): MA[PartialApply2Of3[memo.Comemo, K, V]#ApplyA, T] = maPartial[PartialApply2Of3[memo.Comemo, K, V]#ApplyA].apply[T](a)

  implicit def Tuple2MA[R, A](a: (R, A)): MA[PartialApply1Of2[Tuple2, R]#Apply, A] = maPartial[PartialApply1Of2[Tuple2, R]#Apply](a)

  implicit def Tuple3MA[R, S, A](a: (R, S, A)): MA[PartialApply2Of3[Tuple3, R, S]#Apply, A] = maPartial[PartialApply2Of3[Tuple3, R, S]#Apply](a)

  implicit def Tuple4MA[R, S, T, A](a: (R, S, T, A)): MA[PartialApply3Of4[Tuple4, R, S, T]#Apply, A] = maPartial[PartialApply3Of4[Tuple4, R, S, T]#Apply](a)

  implicit def Tuple5MA[R, S, T, U, A](a: (R, S, T, U, A)): MA[PartialApply4Of5[Tuple5, R, S, T, U]#Apply, A] = maPartial[PartialApply4Of5[Tuple5, R, S, T, U]#Apply](a)

  implicit def Tuple6MA[R, S, T, U, V, A](a: (R, S, T, U, V, A)): MA[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply, A] = maPartial[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply](a)

  implicit def Tuple7MA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W, A)): MA[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply, A] = maPartial[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply](a)

  implicit def Function1MA[R, A](a: R => A) = maPartial[PartialApply1Of2[Function1, R]#Apply](a)

  implicit def Function2MA[R, S, A](a: (R, S) => A): MA[PartialApply2Of3[Function2, R, S]#Apply, A] = maPartial[PartialApply2Of3[Function2, R, S]#Apply](a)

  implicit def Function3MA[R, S, T, A](a: (R, S, T) => A): MA[PartialApply3Of4[Function3, R, S, T]#Apply, A] = maPartial[PartialApply3Of4[Function3, R, S, T]#Apply](a)

  implicit def Function4MA[R, S, T, U, A](a: (R, S, T, U) => A): MA[PartialApply4Of5[Function4, R, S, T, U]#Apply, A] = maPartial[PartialApply4Of5[Function4, R, S, T, U]#Apply](a)

  implicit def Function5MA[R, S, T, U, V, A](a: (R, S, T, U, V) => A): MA[PartialApply5Of6[Function5, R, S, T, U, V]#Apply, A] = maPartial[PartialApply5Of6[Function5, R, S, T, U, V]#Apply](a)

  implicit def Function6MA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W) => A): MA[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply, A] = maPartial[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply](a)

  // todo: Remove this and get errors about "scalaz is not an enclosing class" when trying to rely on ScalazLowPriorityImplicits.ma
  //       Boil the problem down and submit a bug.
  implicit def ListMA[A](a: List[A]): MA[List, A] = ma(a)

  // This must stay as Stream[A] <:< (Int => A). Otherwise, Function1MA and ScalazLowPriorityImplicits.ma are ambiguous on account
  // of specificity and subclass implicit search rules.
  implicit def StreamMA[A](a: Stream[A]): MA[Stream, A] = ma(a)

  implicit def EitherLeftMA[X, A](a: Either.LeftProjection[A, X]) = maPartial[PartialApply1Of2[Either.LeftProjection, X]#Flip](a)

  implicit def EitherRightMA[X, A](a: Either.RightProjection[X, A]): MA[PartialApply1Of2[Either.RightProjection, X]#Apply, A] = maPartial[PartialApply1Of2[Either.RightProjection, X]#Apply](a)

  implicit def ValidationMA[E, A](a: Validation[E, A]): MA[PartialApply1Of2[Validation, E]#Apply, A] = maPartial[PartialApply1Of2[Validation, E]#Apply](a)

  implicit def ValidationFailureMA[A, E](a: Validation.FailureProjection[E, A]) = maPartial[PartialApply1Of2[Validation.FailureProjection, A]#Flip](a)

  // MMA

  import MMA._

  implicit def ContinuationMMA[R, A](a: Continuation[R, Continuation[R, A]]): MMA[PartialApply1Of2[Continuation, R]#Apply, A] = mmaPartial[PartialApply1Of2[Continuation, R]#Apply](a)

  implicit def StateMMA[S, A](a: State[S, State[S, A]]): MMA[PartialApply1Of2[State, S]#Apply, A] = mmaPartial[PartialApply1Of2[State, S]#Apply](a)

  implicit def Tuple2MMA[R, A](a: (R, (R, A))): MMA[PartialApply1Of2[Tuple2, R]#Apply, A] = mmaPartial[PartialApply1Of2[Tuple2, R]#Apply](a)

  implicit def Tuple3MMA[R, S, A](a: (R, S, (R, S, A))): MMA[PartialApply2Of3[Tuple3, R, S]#Apply, A] = mmaPartial[PartialApply2Of3[Tuple3, R, S]#Apply](a)

  implicit def Tuple4MMA[R, S, T, A](a: (R, S, T, (R, S, T, A))): MMA[PartialApply3Of4[Tuple4, R, S, T]#Apply, A] = mmaPartial[PartialApply3Of4[Tuple4, R, S, T]#Apply](a)

  implicit def Tuple5MMA[R, S, T, U, A](a: (R, S, T, U, (R, S, T, U, A))): MMA[PartialApply4Of5[Tuple5, R, S, T, U]#Apply, A] = mmaPartial[PartialApply4Of5[Tuple5, R, S, T, U]#Apply](a)

  implicit def Tuple6MMA[R, S, T, U, V, A](a: (R, S, T, U, V, (R, S, T, U, V, A))): MMA[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply, A] = mmaPartial[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply](a)

  implicit def Tuple7MMA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W, (R, S, T, U, V, W, A))): MMA[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply, A] = mmaPartial[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply](a)

  implicit def Function1MMA[R, A](a: R => R => A): MMA[PartialApply1Of2[Function1, R]#Apply, A] = mmaPartial[PartialApply1Of2[Function1, R]#Apply](a)

  implicit def Function2MMA[R, S, A](a: (R, S) => (R, S) => A): MMA[PartialApply2Of3[Function2, R, S]#Apply, A] = mmaPartial[PartialApply2Of3[Function2, R, S]#Apply](a)

  implicit def Function3MMA[R, S, T, A](a: (R, S, T) => (R, S, T) => A): MMA[PartialApply3Of4[Function3, R, S, T]#Apply, A] = mmaPartial[PartialApply3Of4[Function3, R, S, T]#Apply](a)

  implicit def Function4MMA[R, S, T, U, A](a: (R, S, T, U) => (R, S, T, U) => A): MMA[PartialApply4Of5[Function4, R, S, T, U]#Apply, A] = mmaPartial[PartialApply4Of5[Function4, R, S, T, U]#Apply](a)

  implicit def Function5MMA[R, S, T, U, V, A](a: (R, S, T, U, V) => (R, S, T, U, V) => A): MMA[PartialApply5Of6[Function5, R, S, T, U, V]#Apply, A] = mmaPartial[PartialApply5Of6[Function5, R, S, T, U, V]#Apply](a)

  implicit def Function6MMA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W) => (R, S, T, U, V, W) => A): MMA[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply, A] = mmaPartial[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply](a)

  implicit def ListMMA[A](a: List[List[A]]): MMA[List, A] = mma(a)

  implicit def StreamMMA[A](a: Stream[Stream[A]]): MMA[Stream, A] = mma(a)

  implicit def EitherLeftMMA[X, A](a: Either.LeftProjection[Either.LeftProjection[A, X], X]): MMA[PartialApply1Of2[Either.LeftProjection, X]#Flip, A] = mmaPartial[PartialApply1Of2[Either.LeftProjection, X]#Flip](a)

  implicit def EitherRightMMA[X, A](a: Either.RightProjection[X, Either.RightProjection[X, A]]): MMA[PartialApply1Of2[Either.RightProjection, X]#Apply, A] = mmaPartial[PartialApply1Of2[Either.RightProjection, X]#Apply](a)

  implicit def ValidationMMA[X, A](a: Validation[X, Validation[X, A]]): MMA[PartialApply1Of2[Validation, X]#Apply, A] = mmaPartial[PartialApply1Of2[Validation, X]#Apply](a)

  implicit def ValidationFailureMMA[A, X](a: Validation.FailureProjection[Validation.FailureProjection[A, X], X]): MMA[PartialApply1Of2[Validation.FailureProjection, X]#Flip, A] = mmaPartial[PartialApply1Of2[Validation.FailureProjection, X]#Flip](a)

  // MAB
  import MAB.mab

  implicit def EitherMAB[A, B](a: Either[A, B]): MAB[Either, A, B] = mab[Either](a)

  implicit def Function1MAB[A, B](a: A => B): MAB[Function1, A, B] = mab[Function1](a)

  implicit def Tuple2MAB[A, B](a: (A, B)): MAB[Tuple2, A, B] = mab[Tuple2](a)

  trait KleisliMABApply[M[_]] {
    def apply[A, B](a: Kleisli[M, A, B]): MAB[PartialApplyK[Kleisli, M]#Apply, A, B]
  }

  def KleisliMAB[M[_]] = new KleisliMABApply[M] {
    def apply[A, B](a: Kleisli[M, A, B]): MAB[PartialApplyK[Kleisli, M]#Apply, A, B] = mab[PartialApplyK[Kleisli, M]#Apply](a)
  }

  // todo apply type constructor values to KleisliMAB with implicit

  import java.util._
  import java.util.concurrent._

  implicit def HashMapMAB[A, B](a: HashMap[A, B]): MAB[HashMap, A, B] = mab[HashMap](a)

  implicit def HashtableMAB[A, B](a: Hashtable[A, B]): MAB[Hashtable, A, B] = mab[Hashtable](a)

  implicit def IdentityHashMapMAB[A, B](a: IdentityHashMap[A, B]): MAB[IdentityHashMap, A, B] = mab[IdentityHashMap](a)

  implicit def LinkedHashMapMAB[A, B](a: LinkedHashMap[A, B]): MAB[LinkedHashMap, A, B] = mab[LinkedHashMap](a)

  implicit def TreeMapMAB[A, B](a: TreeMap[A, B]): MAB[TreeMap, A, B] = mab[TreeMap](a)

  implicit def WeakHashMapMAB[A, B](a: WeakHashMap[A, B]): MAB[WeakHashMap, A, B] = mab[WeakHashMap](a)

  implicit def ConcurrentHashMapMAB[A, B](a: ConcurrentHashMap[A, B]): MAB[ConcurrentHashMap, A, B] = mab[ConcurrentHashMap](a)

}
