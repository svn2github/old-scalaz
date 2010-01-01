package scalaz

/**
 * A Zero in type Z provides the identity element for the operation {@link scalaz.Semigroup#append}
 * in the corresponding {@link scalaz.Semigroup} in type Z.
 * <p/>
 * ∀ a in S, append(a, zero) = a
 *
 * @see Monoid@identityLaw
 */
trait Zero[+Z] {
  val zero: Z
}

trait Zeros {
  def zero[Z](z: Z): Zero[Z] = new Zero[Z] {
    val zero = z
  }

  /**
   * @usage ∅[Z]
   * @usage ∅: Z
   */
  def ∅[Z](implicit z: Zero[Z]): Z = z.zero
}

object Zero {
  import Scalaz._
  import xml.{Elem, Node, NodeSeq}
  
  implicit def DigitZero: Zero[Digit] = zero(_0)

  implicit def OrderingZero: Zero[Ordering] = zero(EQ)

  implicit def UnitZero: Zero[Unit] = zero(())

  implicit def StringZero: Zero[String] = zero("")

  implicit def IntZero: Zero[Int] = zero(0)

  implicit def IntMultiplicationZero: Zero[IntMultiplication] = zero(1 ∏)

  implicit def BooleanConjunctionZero: Zero[BooleanConjunction] = zero(true |∧|)

  implicit def BooleanZero: Zero[Boolean] = zero(false)

  implicit def CharZero: Zero[Char] = zero(0.toChar)

  implicit def CharMultiplicationZero: Zero[CharMultiplication] = zero(1.toChar ∏)

  implicit def ByteZero: Zero[Byte] = zero(0.toByte)

  implicit def ByteMultiplicationZero: Zero[ByteMultiplication] = zero(1.toByte ∏)

  implicit def LongZero: Zero[Long] = zero(0L)

  implicit def LongMultiplicationZero: Zero[LongMultiplication] = zero(1L ∏)

  implicit def ShortZero: Zero[Short] = zero(0.toShort)

  implicit def ShortMultiplicationZero: Zero[ShortMultiplication] = zero(1.toShort ∏)

  implicit def FloatZero: Zero[Float] = zero(0F)

  implicit def DoubleZero: Zero[Double] = zero(0D)

  implicit def BigIntegerZero = zero(java.math.BigInteger.valueOf(0))

  implicit def BigIntegerMultiplicationZero = zero(java.math.BigInteger.valueOf(1) ∏)

  implicit def BigIntZero: Zero[BigInt] = zero(BigInt(0))

  implicit def BigIntMutliplicationZero: Zero[BigIntMultiplication] = zero(BigInt(1) ∏)

  implicit def NodeSeqZero: Zero[NodeSeq] = zero(NodeSeq.Empty)

  implicit def NodeZero: Zero[Node] = new Zero[Node] {
    val zero = new Node {
      override def text = null
      override def label = null
      override def child = Nil
    }
  }

  implicit def ElemZero: Zero[Elem] = new Zero[Elem] {
    val zero = new Elem(null, null, scala.xml.Null, xml.TopScope, Nil: _*)
  }

  implicit def ZipStreamZero[A]: Zero[ZipStream[A]] = zero(zip(Stream.Empty))

  implicit def ListZero[A]: Zero[List[A]] = zero(Nil)

  implicit def StreamZero[A]: Zero[Stream[A]] = zero(Stream.Empty)

  implicit def OptionZero[A]: Zero[Option[A]] = zero(None)

  implicit def FirstOptionZero[A]: Zero[FirstOption[A]] = zero(None)

  implicit def LastOptionZero[A]: Zero[LastOption[A]] = zero(None)

  implicit def ArrayZero[A: Manifest]: Zero[Array[A]] = zero(new Array[A](0))

  implicit def GenericArrayZero[A]: Zero[GArray[A]] = zero(new GArray[A](0))

  implicit def EitherLeftZero[A: Zero, B]: Zero[Either.LeftProjection[A, B]] = zero(Left(∅[A]).left)

  implicit def EitherRightZero[A, B: Zero]: Zero[Either.RightProjection[A, B]] = zero(Right(∅[B]).right)

  implicit def Function1ABZero[A, B: Zero]: Zero[A => B] = zero((_: A) => ∅[B])

  implicit def EndoZero[A]: Zero[Endo[A]] = zero(identity(_: A))

  implicit def DualZero[A: Zero]: Zero[Dual[A]] = zero(∅[A] σ)

  implicit def ZeroKleisliZero[M[_],A,B](implicit z: Zero[M[B]]): Zero[Kleisli[M,A,B]] = zero(☆((_ : A) => ∅))

  import concurrent.Strategy
  import concurrent.strategy.Id

  implicit def StrategyZero[A]: Zero[Strategy[A]] = zero(Id.strategy[A])

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListZero[A]: Zero[ArrayList[A]] = zero(new ArrayList[A])

  implicit def JavaHashMapZero[K, V]: Zero[HashMap[K, V]] = zero(new HashMap[K, V])

  implicit def JavaHashSetZero[A]: Zero[HashSet[A]] = zero(new HashSet[A])

  implicit def JavaHashtableZero[K, V]: Zero[Hashtable[K, V]] = zero(new Hashtable[K, V])

  implicit def JavaIdentityHashMapZero[K, V] = zero(new IdentityHashMap[K, V])

  implicit def JavaLinkedHashMapZero[K, V]: Zero[LinkedHashMap[K, V]] = zero(new LinkedHashMap[K, V])

  implicit def JavaLinkedHashSetZero[A]: Zero[LinkedHashSet[A]] = zero(new LinkedHashSet[A])

  implicit def JavaLinkedListZero[A]: Zero[LinkedList[A]] = zero(new LinkedList[A])

  implicit def JavaPriorityQueueZero[A]: Zero[PriorityQueue[A]] = zero(new PriorityQueue[A])

  implicit def JavaStackZero[A]: Zero[Stack[A]] = zero(new Stack[A])

  implicit def JavaTreeMapZero[K, V]: Zero[TreeMap[K, V]] = zero(new TreeMap[K, V])

  implicit def JavaTreeSetZero[A]: Zero[TreeSet[A]] = zero(new TreeSet[A])

  implicit def JavaVectorZero[A]: Zero[Vector[A]] = zero(new Vector[A])

  implicit def JavaWeakHashMapZero[K, V]: Zero[WeakHashMap[K, V]] = zero(new WeakHashMap[K, V])

  implicit def JavaArrayBlockingQueueZero[A]: Zero[ArrayBlockingQueue[A]] = zero(new ArrayBlockingQueue[A](0))

  implicit def JavaConcurrentHashMapZero[K, V]: Zero[ConcurrentHashMap[K, V]] = zero(new ConcurrentHashMap[K, V])

  implicit def JavaConcurrentLinkedQueueZero[A]: Zero[ConcurrentLinkedQueue[A]] = zero(new ConcurrentLinkedQueue[A])

  implicit def JavaCopyOnWriteArrayListZero[A]: Zero[CopyOnWriteArrayList[A]] = zero(new CopyOnWriteArrayList[A])

  implicit def JavaCopyOnWriteArraySetZero[A]: Zero[CopyOnWriteArraySet[A]] = zero(new CopyOnWriteArraySet[A])

  implicit def JavaLinkedBlockingQueueZero[A]: Zero[LinkedBlockingQueue[A]] = zero(new LinkedBlockingQueue[A])

  implicit def JavaSynchronousQueueZero[A]: Zero[SynchronousQueue[A]] = zero(new SynchronousQueue[A])
}