package scalaz

trait Semigroup[S] {
  def append(s1: S, s2: => S): S
}

trait Semigroups {
  def semigroup[S](f: (S, => S) => S) = new Semigroup[S] {
    def append(s1: S, s2: => S) = f(s1, s2)
  }
}

object Semigroup {
  import Scalaz._
  import xml.{Elem, Node, NodeSeq}

  implicit val DigitSemigroup: Semigroup[Digit] = semigroup((a, b) => a.toInt + b.toInt)

  implicit val OrderingSemigroup: Semigroup[Ordering] = semigroup {
    case (EQ, a) => a
    case (LT, _) => LT
    case (GT, _) => GT
  }

  implicit val UnitSemigroup: Semigroup[Unit] = semigroup((_, _) => ())

  implicit val StringSemigroup: Semigroup[String] = semigroup(_ + _)

  implicit val IntSemigroup: Semigroup[Int] = semigroup(_ + _)

  implicit val IntMultiplicationSemigroup: Semigroup[IntMultiplication] = semigroup(_ * _ ∏)

  implicit val BooleanConjunctionSemigroup: Semigroup[BooleanConjunction] = semigroup(_ && _ |∧|)

  implicit val BooleanSemigroup: Semigroup[Boolean] = semigroup((a, b) => (a || b))

  implicit val CharSemigroup: Semigroup[Char] = semigroup((a, b) => (a + b).toChar)

  implicit val CharMultiplicationSemigroup: Semigroup[CharMultiplication] = semigroup((a, b) => (a * b).toChar ∏)

  implicit val ByteSemigroup: Semigroup[Byte] = semigroup((a, b) => (a + b).toByte)

  implicit val ByteMultiplicationSemigroup: Semigroup[ByteMultiplication] = semigroup((a, b) => (a * b).toByte ∏)

  implicit val LongSemigroup: Semigroup[Long] = semigroup((a, b) => (a + b).toLong)

  implicit val LongMultiplicationSemigroup: Semigroup[LongMultiplication] = semigroup((a, b) => (a * b).toLong ∏)

  implicit val ShortSemigroup: Semigroup[Short] = semigroup((a, b) => (a + b).toShort)

  implicit val ShortMultiplicationSemigroup: Semigroup[ShortMultiplication] = semigroup((a, b) => (a * b).toShort ∏)

  implicit val FloatSemigroup: Semigroup[Float] = semigroup((a, b) => (a + b).toFloat)

  implicit val DoubleSemigroup: Semigroup[Double] = semigroup((a, b) => (a + b).toDouble)

  implicit val BigIntegerSemigroup: Semigroup[java.math.BigInteger] = semigroup(_ add _)

  implicit val BigIntegerMutliplicationSemigroup: Semigroup[BigIntegerMultiplication] = semigroup(_.value multiply _.value ∏)

  implicit val BigIntSemigroup: Semigroup[BigInt] = semigroup(_ + _)

  implicit val BigIntMultiplicationSemigroup: Semigroup[BigIntMultiplication] = semigroup(_.value * _.value ∏)

  implicit val NodeSeqSemigroup: Semigroup[NodeSeq] = semigroup(_ ++ _)

  implicit def NonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] = semigroup(_.list <::: _)
  
  implicit def ZipStreamSemigroup[A]: Semigroup[ZipStream[A]] = semigroup(_.value append _.value ʐ)

  implicit def ListSemigroup[A]: Semigroup[List[A]] = semigroup(_ ::: _)

  implicit def StreamSemigroup[A]: Semigroup[Stream[A]] = semigroup(_ append _)

  implicit def OptionSemigroup[A]: Semigroup[Option[A]] = semigroup((a, b) => if (a.isDefined) a else b)

  implicit def FirstSemigroup[A]: Semigroup[FirstOption[A]] = semigroup((a, b) => if (a.isDefined) a else b)

  implicit def LastSemigroup[A]: Semigroup[LastOption[A]] = semigroup((a, b) => if (a.isDefined) a else b)

  implicit def ArraySemigroup[A: Manifest]: Semigroup[Array[A]] = semigroup(Array.concat(_, _))

  implicit def GenericArraySemigroup[A]: Semigroup[GArray[A]] = semigroup(_ ++ _)

  implicit def EitherLeftSemigroup[A, B]: Semigroup[Either.LeftProjection[A, B]] = semigroup((a, b) => if (a.e.isRight) a else b)

  implicit def EitherRightSemigroup[A, B]: Semigroup[Either.RightProjection[B, A]] = semigroup((a, b) => if (a.e.isLeft) a else b)

  implicit def Function1ABSemigroup[A, B](implicit s: Semigroup[B]): Semigroup[A => B] = semigroup((a1, a2) => a => s append (a1(a), a2.apply(a)))

  implicit def EndoSemigroup[A]: Semigroup[Endo[A]] = semigroup((x, y) => ((a: A) => x(y.apply(a))))

  implicit def DualSemigroup[A](implicit sa: Semigroup[A]): Semigroup[Dual[A]] =
    semigroup((x, y) => sa.append(y.value, x.value))

  import concurrent.Strategy
  
  implicit def StrategySemigroup[A]: Semigroup[Strategy[A]] = semigroup((x, y) => ((a: () => A) => x(y.apply(a))))

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListSemigroup[A]: Semigroup[ArrayList[A]] = semigroup((a, b) => {
    val k = a.clone.asInstanceOf[ArrayList[A]]
    k addAll b
    k
  })

  implicit def JavaLinkedListSemigroup[A]: Semigroup[LinkedList[A]] = semigroup((a, b) => {
    val k = a.clone.asInstanceOf[LinkedList[A]]
    k addAll b
    k
  })

  implicit def JavaPriorityQueueSemigroup[A]: Semigroup[PriorityQueue[A]] = semigroup((a, b) => {
    val k = new PriorityQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaStackSemigroup[A]: Semigroup[Stack[A]] = semigroup((a, b) => {
    val k = a.clone.asInstanceOf[Stack[A]]
    k addAll b
    k
  })

  implicit def JavaVectorSemigroup[A]: Semigroup[Vector[A]] = semigroup((a, b) => {
    val k = a.clone.asInstanceOf[Vector[A]]
    k addAll b
    k
  })

  implicit def JavaArrayBlockingQueueSemigroup[A]: Semigroup[ArrayBlockingQueue[A]] = semigroup((a, b) => {
    val k = new ArrayBlockingQueue[A](a.remainingCapacity + b.remainingCapacity)
    k addAll a
    k addAll b
    k
  })

  implicit def JavaConcurrentLinkedQueueSemigroup[A]: Semigroup[ConcurrentLinkedQueue[A]] = semigroup((a, b) => {
    val k = new ConcurrentLinkedQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaCopyOnWriteArrayListSemigroup[A]: Semigroup[CopyOnWriteArrayList[A]] = semigroup((a, b) => {
    val k = a.clone.asInstanceOf[CopyOnWriteArrayList[A]]
    k addAll b
    k
  })

  implicit def JavaLinkedBlockingQueueSemigroup[A]: Semigroup[LinkedBlockingQueue[A]] = semigroup((a, b) => {
    val k = new LinkedBlockingQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaSynchronousQueueSemigroup[A]: Semigroup[SynchronousQueue[A]] = semigroup((a, b) => {
    val k = new SynchronousQueue[A]
    k addAll a
    k addAll b
    k
  })
}
