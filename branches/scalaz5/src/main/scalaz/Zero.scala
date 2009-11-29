package scalaz

trait Zero[+Z] {
  val zero: Z
}

trait Zeros {
  def zero[Z](z: Z): Zero[Z] = new Zero[Z] {
    val zero = z
  }

  def ∅[Z](implicit z: Zero[Z]): Z = z.zero
}

object Zero {
  import Scalaz._
  import xml.{Elem, Node, NodeSeq}
  
  implicit val DigitZero: Zero[Digit] = zero(_0)

  implicit val OrderingZero: Zero[Ordering] = zero(EQ)

  implicit val UnitZero: Zero[Unit] = zero(())

  implicit val StringZero: Zero[String] = zero("")

  implicit val IntZero: Zero[Int] = zero(0)

  implicit val IntMultiplicationZero: Zero[IntMultiplication] = zero(1 ∏)

  implicit val BooleanConjunctionZero: Zero[BooleanConjunction] = zero(true |∧|)

  implicit val BooleanZero: Zero[Boolean] = zero(false)

  implicit val CharZero: Zero[Char] = zero(0.toChar)

  implicit val CharMultiplicationZero: Zero[CharMultiplication] = zero(1.toChar ∏)

  implicit val ByteZero: Zero[Byte] = zero(0.toByte)

  implicit val ByteMultiplicationZero: Zero[ByteMultiplication] = zero(1.toByte ∏)

  implicit val LongZero: Zero[Long] = zero(0L)

  implicit val LongMultiplicationZero: Zero[LongMultiplication] = zero(1L ∏)

  implicit val ShortZero: Zero[Short] = zero(0.toShort)

  implicit val ShortMultiplicationZero: Zero[ShortMultiplication] = zero(1.toShort ∏)

  implicit val FloatZero: Zero[Float] = zero(0F)

  implicit val DoubleZero: Zero[Double] = zero(0D)

  implicit val BigIntegerZero = zero(java.math.BigInteger.valueOf(0))

  implicit val BigIntegerMultiplicationZero = zero(java.math.BigInteger.valueOf(1) ∏)

  implicit val BigIntZero: Zero[BigInt] = zero(BigInt(0))

  implicit val BigIntMutliplicationZero: Zero[BigIntMultiplication] = zero(BigInt(1) ∏)

  implicit val NodeSeqZero: Zero[NodeSeq] = zero(NodeSeq.Empty)

  implicit val NodeZero: Zero[Node] = new Zero[Node] {
    val zero = new Node {
      override def text = null
      override def label = null
      override def child = Nil
    }
  }

  implicit def ElemZero: Zero[Elem] = new Zero[Elem] {
    val zero = new Elem(null, null, scala.xml.Null, xml.TopScope, Nil: _*)
  }

  // todo implicit def ZipStreamZero[A] = zero[ZipStream[A]](ZipStream.zip(Stream.empty))

  implicit def ListZero[A]: Zero[List[A]] = zero(Nil)

  implicit def StreamZero[A]: Zero[Stream[A]] = zero(Stream.empty)

  implicit def OptionZero[A]: Zero[Option[A]] = zero(None)

  implicit def ArrayZero[A: Manifest]: Zero[Array[A]] = zero(new Array[A](0))

  implicit def EitherLeftZero[A, B](implicit za: Zero[A]): Zero[Either.LeftProjection[A, B]] = zero(Left(za.zero).left)

  implicit def EitherRightZero[A, B](implicit za: Zero[A]): Zero[Either.RightProjection[B, A]] = zero(Right(za.zero).right)

  implicit def Function1ABZero[A, B](implicit zb: Zero[B]): Zero[A => B] = zero((_: A) => zb.zero)

  implicit def EndoZero[A]: Zero[Endo[A]] = zero(identity(_: A))

  implicit def DualZero[A](implicit za: Zero[A]): Zero[Dual[A]] = zero(za.zero σ)

  // todo implicit def StrategyZero[A] = zero[Strategy[A]](concurrent.strategies.Id.strategy[A])

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