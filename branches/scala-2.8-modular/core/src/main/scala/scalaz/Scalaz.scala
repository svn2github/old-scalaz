package scalaz

object Scalaz {
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

  import MA.ma

  implicit def IdentityMA[A](a: Identity[A]): MA[Identity, A] = ma[Identity](a)

  implicit def ContinuationMA[R, A](a: Continuation[R, A]): MA[PartialApply1Of2[Continuation, R]#Apply, A] = ma[PartialApply1Of2[Continuation, R]#Apply](a)

  implicit def NonEmptyListMA[A](a: NonEmptyList[A]): MA[NonEmptyList, A] = ma[NonEmptyList](a)

  implicit def DListMA[A](a: DList[A]): MA[DList, A] = ma[DList](a)

  implicit def StateMA[S, A](a: State[S, A]): MA[PartialApply1Of2[State, S]#Apply, A] = ma[PartialApply1Of2[State, S]#Apply](a)

  implicit def EqualMA[A](a: Equal[A]): MA[Equal, A] = ma[Equal](a)

  implicit def OrderMA[A](a: Order[A]): MA[Order, A] = ma[Order](a)

  implicit def ShowMA[A](a: Show[A]): MA[Show, A] = ma[Show](a)

  implicit def ZipStreamMA[A](a: ZipStream[A]): MA[ZipStream, A] = ma[ZipStream](a)

  implicit def ComemoMA[T, K, V](a: memo.Comemo[T, K, V]): MA[PartialApply2Of3[memo.Comemo, K, V]#ApplyA, T] = ma[PartialApply2Of3[memo.Comemo, K, V]#ApplyA].apply[T](a)

  implicit def MetricSpaceStreamMA[A](a: MetricSpace[A]): MA[MetricSpace, A] = ma[MetricSpace](a)

  implicit def Tuple1MA[A](a: Tuple1[A]): MA[Tuple1, A] = ma[Tuple1](a)

  implicit def Tuple2MA[R, A](a: (R, A)): MA[PartialApply1Of2[Tuple2, R]#Apply, A] = ma[PartialApply1Of2[Tuple2, R]#Apply](a)

  implicit def Tuple3MA[R, S, A](a: (R, S, A)): MA[PartialApply2Of3[Tuple3, R, S]#Apply, A] = ma[PartialApply2Of3[Tuple3, R, S]#Apply](a)

  implicit def Tuple4MA[R, S, T, A](a: (R, S, T, A)): MA[PartialApply3Of4[Tuple4, R, S, T]#Apply, A] = ma[PartialApply3Of4[Tuple4, R, S, T]#Apply](a)

  implicit def Tuple5MA[R, S, T, U, A](a: (R, S, T, U, A)): MA[PartialApply4Of5[Tuple5, R, S, T, U]#Apply, A] = ma[PartialApply4Of5[Tuple5, R, S, T, U]#Apply](a)

  implicit def Tuple6MA[R, S, T, U, V, A](a: (R, S, T, U, V, A)): MA[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply, A] = ma[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply](a)

  implicit def Tuple7MA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W, A)): MA[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply, A] = ma[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply](a)

  implicit def Function0MA[A](a: Function0[A]): MA[Function0, A] = ma[Function0](a)

  implicit def Function1MA[R, A](a: R => A) = ma[PartialApply1Of2[Function1, R]#Apply](a)

  implicit def Function2MA[R, S, A](a: (R, S) => A): MA[PartialApply2Of3[Function2, R, S]#Apply, A] = ma[PartialApply2Of3[Function2, R, S]#Apply](a)

  implicit def Function3MA[R, S, T, A](a: (R, S, T) => A): MA[PartialApply3Of4[Function3, R, S, T]#Apply, A] = ma[PartialApply3Of4[Function3, R, S, T]#Apply](a)

  implicit def Function4MA[R, S, T, U, A](a: (R, S, T, U) => A): MA[PartialApply4Of5[Function4, R, S, T, U]#Apply, A] = ma[PartialApply4Of5[Function4, R, S, T, U]#Apply](a)

  implicit def Function5MA[R, S, T, U, V, A](a: (R, S, T, U, V) => A): MA[PartialApply5Of6[Function5, R, S, T, U, V]#Apply, A] = ma[PartialApply5Of6[Function5, R, S, T, U, V]#Apply](a)

  implicit def Function6MA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W) => A): MA[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply, A] = ma[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply](a)

  implicit def ListMA[A](a: List[A]): MA[List, A] = ma[List](a)

  implicit def StreamMA[A](a: Stream[A]): MA[Stream, A] = ma[Stream](a)

  implicit def OptionMA[A](a: Option[A]): MA[Option, A] = ma[Option](a)

  implicit def ArrayMA[A](a: Array[A]): MA[Array, A] = ma[Array](a)

  implicit def EitherLeftMA[X, A](a: Either.LeftProjection[A, X]) = ma[PartialApply1Of2[Either.LeftProjection, X]#Flip](a)

  implicit def EitherRightMA[X, A](a: Either.RightProjection[X, A]): MA[PartialApply1Of2[Either.RightProjection, X]#Apply, A] = ma[PartialApply1Of2[Either.RightProjection, X]#Apply](a)

  implicit def ValidationMA[E, A](a: Validation[E, A]): MA[PartialApply1Of2[Validation, E]#Apply, A] = ma[PartialApply1Of2[Validation, E]#Apply](a)

  implicit def ValidationFailureMA[A, E](a: Validation.FailureProjection[E, A]) = ma[PartialApply1Of2[Validation.FailureProjection, A]#Flip](a)

  implicit def ZipperMA[A](a: Zipper[A]): MA[Zipper, A] = ma[Zipper](a)

  implicit def EndoMA[A](a: Endo[A]): MA[Endo, A] = ma[Endo](a)

  implicit def TreeMA[A](a: Tree[A]): MA[Tree, A] = ma[Tree](a)

  implicit def TreeLocMA[A](a: TreeLoc[A]): MA[TreeLoc, A] = ma[TreeLoc](a)

  import concurrent._
  implicit def StrategyMA[A](a: Strategy[A]): MA[Strategy, A] = ma[Strategy](a)

  implicit def ActorMA[A](a: Actor[A]): MA[Actor, A] = ma[Actor](a)

  implicit def PromiseMA[A](a: Promise[A]): MA[Promise, A] = ma[Promise](a)

  implicit def EffectMA[A](a: Effect[A]): MA[Effect, A] = ma[Effect](a)

  import java.util._
  import java.util.concurrent._

  implicit def ArrayListMA[A](a: ArrayList[A]): MA[ArrayList, A] = ma[ArrayList](a)

  implicit def HashSetMA[A](a: HashSet[A]): MA[HashSet, A] = ma[HashSet](a)

  implicit def LinkedHashSetMA[A](a: LinkedHashSet[A]): MA[LinkedHashSet, A] = ma[LinkedHashSet](a)

  implicit def LinkedListMA[A](a: LinkedList[A]): MA[LinkedList, A] = ma[LinkedList](a)

  implicit def PriorityQueueMA[A](a: PriorityQueue[A]): MA[PriorityQueue, A] = ma[PriorityQueue](a)

  implicit def StackMA[A](a: Stack[A]): MA[Stack, A] = ma[Stack](a)

  implicit def TreeSetMA[A](a: TreeSet[A]): MA[TreeSet, A] = ma[TreeSet](a)

  implicit def VectorMA[A](a: Vector[A]): MA[Vector, A] = ma[Vector](a)

  implicit def ArrayBlockingQueueMA[A](a: ArrayBlockingQueue[A]): MA[ArrayBlockingQueue, A] = ma[ArrayBlockingQueue](a)

  implicit def ConcurrentLinkedQueueMA[A](a: ConcurrentLinkedQueue[A]): MA[ConcurrentLinkedQueue, A] = ma[ConcurrentLinkedQueue](a)

  implicit def CopyOnWriteArrayListMA[A](a: CopyOnWriteArrayList[A]): MA[CopyOnWriteArrayList, A] = ma[CopyOnWriteArrayList](a)

  implicit def CopyOnWriteArraySetMA[A](a: CopyOnWriteArraySet[A]): MA[CopyOnWriteArraySet, A] = ma[CopyOnWriteArraySet](a)

  implicit def LinkedBlockingQueueMA[A](a: LinkedBlockingQueue[A]): MA[LinkedBlockingQueue, A] = ma[LinkedBlockingQueue](a)

  implicit def PriorityBlockingQueueMA[A](a: PriorityBlockingQueue[A]): MA[PriorityBlockingQueue, A] = ma[PriorityBlockingQueue](a)

  implicit def SynchronousQueueMA[A](a: SynchronousQueue[A]): MA[SynchronousQueue, A] = ma[SynchronousQueue](a)

  // MMAga

  import MMA.mma

  implicit def IdentityMMA[A](a: Identity[Identity[A]]): MMA[Identity, A] = mma[Identity](a)

  implicit def ContinuationMMA[R, A](a: Continuation[R, Continuation[R, A]]): MMA[PartialApply1Of2[Continuation, R]#Apply, A] = mma[PartialApply1Of2[Continuation, R]#Apply](a)

  implicit def NonEmptyListMMA[A](a: NonEmptyList[NonEmptyList[A]]): MMA[NonEmptyList, A] = mma[NonEmptyList](a)

  implicit def DListMMA[A](a: DList[DList[A]]): MMA[DList, A] = mma[DList](a)

  implicit def StateMMA[S, A](a: State[S, State[S, A]]): MMA[PartialApply1Of2[State, S]#Apply, A] = mma[PartialApply1Of2[State, S]#Apply](a)

  implicit def EqualMMA[A](a: Equal[Equal[A]]): MMA[Equal, A] = mma[Equal](a)

  implicit def OrderMMA[A](a: Order[Order[A]]): MMA[Order, A] = mma[Order](a)

  implicit def ShowMMA[A](a: Show[Show[A]]): MMA[Show, A] = mma[Show](a)

  implicit def ZipStreamMMA[A](a: ZipStream[ZipStream[A]]): MMA[ZipStream, A] = mma[ZipStream](a)

  implicit def MetricSpaceMMA[A](a: MetricSpace[MetricSpace[A]]): MMA[MetricSpace, A] = mma[MetricSpace](a)

  implicit def Tuple1MMA[A](a: Tuple1[Tuple1[A]]): MMA[Tuple1, A] = mma[Tuple1](a)

  implicit def Tuple2MMA[R, A](a: (R, (R, A))): MMA[PartialApply1Of2[Tuple2, R]#Apply, A] = mma[PartialApply1Of2[Tuple2, R]#Apply](a)

  implicit def Tuple3MMA[R, S, A](a: (R, S, (R, S, A))): MMA[PartialApply2Of3[Tuple3, R, S]#Apply, A] = mma[PartialApply2Of3[Tuple3, R, S]#Apply](a)

  implicit def Tuple4MMA[R, S, T, A](a: (R, S, T, (R, S, T, A))): MMA[PartialApply3Of4[Tuple4, R, S, T]#Apply, A] = mma[PartialApply3Of4[Tuple4, R, S, T]#Apply](a)

  implicit def Tuple5MMA[R, S, T, U, A](a: (R, S, T, U, (R, S, T, U, A))): MMA[PartialApply4Of5[Tuple5, R, S, T, U]#Apply, A] = mma[PartialApply4Of5[Tuple5, R, S, T, U]#Apply](a)

  implicit def Tuple6MMA[R, S, T, U, V, A](a: (R, S, T, U, V, (R, S, T, U, V, A))): MMA[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply, A] = mma[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply](a)

  implicit def Tuple7MMA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W, (R, S, T, U, V, W, A))): MMA[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply, A] = mma[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply](a)

  implicit def Function0MMA[A](a: Function0[Function0[A]]): MMA[Function0, A] = mma[Function0](a)

  implicit def Function1MMA[R, A](a: R => R => A): MMA[PartialApply1Of2[Function1, R]#Apply, A] = mma[PartialApply1Of2[Function1, R]#Apply](a)

  implicit def Function2MMA[R, S, A](a: (R, S) => (R, S) => A): MMA[PartialApply2Of3[Function2, R, S]#Apply, A] = mma[PartialApply2Of3[Function2, R, S]#Apply](a)

  implicit def Function3MMA[R, S, T, A](a: (R, S, T) => (R, S, T) => A): MMA[PartialApply3Of4[Function3, R, S, T]#Apply, A] = mma[PartialApply3Of4[Function3, R, S, T]#Apply](a)

  implicit def Function4MMA[R, S, T, U, A](a: (R, S, T, U) => (R, S, T, U) => A): MMA[PartialApply4Of5[Function4, R, S, T, U]#Apply, A] = mma[PartialApply4Of5[Function4, R, S, T, U]#Apply](a)

  implicit def Function5MMA[R, S, T, U, V, A](a: (R, S, T, U, V) => (R, S, T, U, V) => A): MMA[PartialApply5Of6[Function5, R, S, T, U, V]#Apply, A] = mma[PartialApply5Of6[Function5, R, S, T, U, V]#Apply](a)

  implicit def Function6MMA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W) => (R, S, T, U, V, W) => A): MMA[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply, A] = mma[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply](a)

  implicit def ListMMA[A](a: List[List[A]]): MMA[List, A] = mma[List](a)

  implicit def StreamMMA[A](a: Stream[Stream[A]]): MMA[Stream, A] = mma[Stream](a)

  implicit def OptionMMA[A](a: Option[Option[A]]): MMA[Option, A] = mma[Option](a)

  implicit def ArrayMMA[A](a: Array[Array[A]]): MMA[Array, A] = mma[Array](a)

  implicit def EitherLeftMMA[X, A](a: Either.LeftProjection[Either.LeftProjection[A, X], X]): MMA[PartialApply1Of2[Either.LeftProjection, X]#Flip, A] = mma[PartialApply1Of2[Either.LeftProjection, X]#Flip](a)

  implicit def EitherRightMMA[X, A](a: Either.RightProjection[X, Either.RightProjection[X, A]]): MMA[PartialApply1Of2[Either.RightProjection, X]#Apply, A] = mma[PartialApply1Of2[Either.RightProjection, X]#Apply](a)

  implicit def ValidationMMA[X, A](a: Validation[X, Validation[X, A]]): MMA[PartialApply1Of2[Validation, X]#Apply, A] = mma[PartialApply1Of2[Validation, X]#Apply](a)

  implicit def ValidationFailureMMA[A, X](a: Validation.FailureProjection[Validation.FailureProjection[A, X], X]): MMA[PartialApply1Of2[Validation.FailureProjection, X]#Flip, A] = mma[PartialApply1Of2[Validation.FailureProjection, X]#Flip](a)

  implicit def ZipperMMA[A](a: Zipper[Zipper[A]]): MMA[Zipper, A] = mma[Zipper](a)

  implicit def EndoMMA[A](a: Endo[Endo[A]]): MMA[Endo, A] = mma[Endo](a)

  implicit def TreeMMA[A](a: Tree[Tree[A]]): MMA[Tree, A] = mma[Tree](a)

  implicit def TreeLocMMA[A](a: TreeLoc[TreeLoc[A]]): MMA[TreeLoc, A] = mma[TreeLoc](a)

  import java.util._
  import java.util.concurrent._

  implicit def ArrayListMMA[A](a: ArrayList[ArrayList[A]]): MMA[ArrayList, A] = mma[ArrayList](a)

  implicit def HashSetMMA[A](a: HashSet[HashSet[A]]): MMA[HashSet, A] = mma[HashSet](a)

  implicit def LinkedHashSetMMA[A](a: LinkedHashSet[LinkedHashSet[A]]): MMA[LinkedHashSet, A] = mma[LinkedHashSet](a)

  implicit def LinkedListMMA[A](a: LinkedList[LinkedList[A]]): MMA[LinkedList, A] = mma[LinkedList](a)

  implicit def PriorityQueueMMA[A](a: PriorityQueue[PriorityQueue[A]]): MMA[PriorityQueue, A] = mma[PriorityQueue](a)

  implicit def StackMMA[A](a: Stack[Stack[A]]): MMA[Stack, A] = mma[Stack](a)

  implicit def TreeSetMMA[A](a: TreeSet[TreeSet[A]]): MMA[TreeSet, A] = mma[TreeSet](a)

  implicit def VectorMMA[A](a: Vector[Vector[A]]): MMA[Vector, A] = mma[Vector](a)

  implicit def ArrayBlockingQueueMMA[A](a: ArrayBlockingQueue[ArrayBlockingQueue[A]]): MMA[ArrayBlockingQueue, A] = mma[ArrayBlockingQueue](a)

  implicit def ConcurrentLinkedQueueMMA[A](a: ConcurrentLinkedQueue[ConcurrentLinkedQueue[A]]): MMA[ConcurrentLinkedQueue, A] = mma[ConcurrentLinkedQueue](a)

  implicit def CopyOnWriteArrayListMMA[A](a: CopyOnWriteArrayList[CopyOnWriteArrayList[A]]): MMA[CopyOnWriteArrayList, A] = mma[CopyOnWriteArrayList](a)

  implicit def CopyOnWriteArraySetMMA[A](a: CopyOnWriteArraySet[CopyOnWriteArraySet[A]]): MMA[CopyOnWriteArraySet, A] = mma[CopyOnWriteArraySet](a)

  implicit def LinkedBlockingQueueMMA[A](a: LinkedBlockingQueue[LinkedBlockingQueue[A]]): MMA[LinkedBlockingQueue, A] = mma[LinkedBlockingQueue](a)

  implicit def PriorityBlockingQueueMMA[A](a: PriorityBlockingQueue[PriorityBlockingQueue[A]]): MMA[PriorityBlockingQueue, A] = mma[PriorityBlockingQueue](a)

  implicit def SynchronousQueueMMA[A](a: SynchronousQueue[SynchronousQueue[A]]): MMA[SynchronousQueue, A] = mma[SynchronousQueue](a)

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
