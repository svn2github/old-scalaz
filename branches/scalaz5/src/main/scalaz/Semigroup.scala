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

  implicit val DigitSemigroup: Semigroup[Digit] = semigroup[Digit]((a, b) => a.toInt + b.toInt)

  implicit val OrderingSemigroup: Semigroup[Ordering] = semigroup[Ordering] {
    case (EQ, a) => a
    case (LT, _) => LT
    case (GT, _) => GT
  }

  implicit val UnitSemigroup: Semigroup[Unit] = semigroup[Unit]((_, _) => ())

  implicit val StringSemigroup: Semigroup[String] = semigroup[String](_ + _)

  implicit val IntSemigroup: Semigroup[Int] = semigroup[Int](_ + _)

  implicit val IntMultiplicationSemigroup: Semigroup[IntMultiplication] = semigroup[IntMultiplication](_ * _ ∏)

  implicit val BooleanConjunctionSemigroup: Semigroup[BooleanConjunction] = semigroup[BooleanConjunction](_ && _ |∧|)

  implicit val BooleanSemigroup: Semigroup[Boolean] = semigroup[Boolean]((a, b) => (a || b))

  implicit val CharSemigroup: Semigroup[Char] = semigroup[Char]((a, b) => (a + b).toChar)

  implicit val CharMultiplicationSemigroup: Semigroup[CharMultiplication] = semigroup[CharMultiplication]((a, b) => (a * b).toChar ∏)

  implicit val ByteSemigroup: Semigroup[Byte] = semigroup[Byte]((a, b) => (a + b).toByte)

  implicit val ByteMultiplicationSemigroup: Semigroup[ByteMultiplication] = semigroup[ByteMultiplication]((a, b) => (a * b).toByte ∏)

  implicit val LongSemigroup: Semigroup[Long] = semigroup[Long]((a, b) => (a + b).toLong)

  implicit val LongMultiplicationSemigroup: Semigroup[LongMultiplication] = semigroup[LongMultiplication]((a, b) => (a * b).toLong ∏)

  implicit val ShortSemigroup: Semigroup[Short] = semigroup[Short]((a, b) => (a + b).toShort)

  implicit val ShortMultiplicationSemigroup: Semigroup[ShortMultiplication] = semigroup[ShortMultiplication]((a, b) => (a * b).toShort ∏)

  implicit val FloatSemigroup: Semigroup[Float] = semigroup[Float]((a, b) => (a + b).toFloat)

  implicit val DoubleSemigroup: Semigroup[Double] = semigroup[Double]((a, b) => (a + b).toDouble)

  implicit val BigIntegerSemigroup: Semigroup[java.math.BigInteger] = semigroup[java.math.BigInteger](_ add _)

  implicit val BigIntegerMutliplicationSemigroup: Semigroup[BigIntegerMultiplication] = semigroup[BigIntegerMultiplication](_.value multiply _.value ∏)

  implicit val BigIntSemigroup: Semigroup[BigInt] = semigroup[BigInt](_ + _)

  implicit val BigIntMultiplicationSemigroup: Semigroup[BigIntMultiplication] = semigroup[BigIntMultiplication](_.value * _.value ∏)

  implicit val NodeSeqSemigroup: Semigroup[NodeSeq] = semigroup[NodeSeq](_ ++ _)

  implicit def NonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] = semigroup[NonEmptyList[A]](_.list <::: _)

  // todo implicit def ZipStreamSemigroup[A] = semigroup[ZipStream[A]](_.value append _.value |!|)

  implicit def ListSemigroup[A]: Semigroup[List[A]] = semigroup[List[A]](_ ::: _)

  implicit def StreamSemigroup[A]: Semigroup[Stream[A]] = semigroup[Stream[A]](_ append _)

  implicit def OptionSemigroup[A]: Semigroup[Option[A]] = semigroup[Option[A]]((a, b) => if (a.isDefined) a else b)

  implicit def ArraySemigroup[A: Manifest]: Semigroup[Array[A]] = semigroup[Array[A]](Array.concat(_, _))

  implicit def EitherLeftSemigroup[A, B]: Semigroup[Either.LeftProjection[A, B]] = semigroup[Either.LeftProjection[A, B]]((a, b) => if (a.e.isRight) a else b)

  implicit def EitherRightSemigroup[A, B]: Semigroup[Either.RightProjection[B, A]] = semigroup[Either.RightProjection[B, A]]((a, b) => if (a.e.isLeft) a else b)

  implicit def Function1ABSemigroup[A, B](implicit s: Semigroup[B]): Semigroup[A => B] = semigroup[A => B]((a1, a2) => a => s append (a1(a), a2.apply(a)))

  implicit def EndoSemigroup[A]: Semigroup[Endo[A]] = semigroup[Endo[A]]((x, y) => ((a: A) => x(y.apply(a))))

  implicit def DualSemigroup[A](implicit sa: Semigroup[A]): Semigroup[Dual[A]] =
    semigroup[Dual[A]]((x, y) => dual(sa.append(y.value, x.value)))

  // todo  implicit def StrategySemigroup[A] = semigroup[Strategy[A]]((x, y) => ((a: () => A) => x(y.apply(a))))

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListSemigroup[A]: Semigroup[ArrayList[A]] = semigroup[ArrayList[A]]((a, b) => {
    val k = a.clone.asInstanceOf[ArrayList[A]]
    k addAll b
    k
  })

  implicit def JavaLinkedListSemigroup[A]: Semigroup[LinkedList[A]] = semigroup[LinkedList[A]]((a, b) => {
    val k = a.clone.asInstanceOf[LinkedList[A]]
    k addAll b
    k
  })

  implicit def JavaPriorityQueueSemigroup[A]: Semigroup[PriorityQueue[A]] = semigroup[PriorityQueue[A]]((a, b) => {
    val k = new PriorityQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaStackSemigroup[A]: Semigroup[Stack[A]] = semigroup[Stack[A]]((a, b) => {
    val k = a.clone.asInstanceOf[Stack[A]]
    k addAll b
    k
  })

  implicit def JavaVectorSemigroup[A]: Semigroup[Vector[A]] = semigroup[Vector[A]]((a, b) => {
    val k = a.clone.asInstanceOf[Vector[A]]
    k addAll b
    k
  })

  implicit def JavaArrayBlockingQueueSemigroup[A]: Semigroup[ArrayBlockingQueue[A]] = semigroup[ArrayBlockingQueue[A]]((a, b) => {
    val k = new ArrayBlockingQueue[A](a.remainingCapacity + b.remainingCapacity)
    k addAll a
    k addAll b
    k
  })

  implicit def JavaConcurrentLinkedQueueSemigroup[A]: Semigroup[ConcurrentLinkedQueue[A]] = semigroup[ConcurrentLinkedQueue[A]]((a, b) => {
    val k = new ConcurrentLinkedQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaCopyOnWriteArrayListSemigroup[A]: Semigroup[CopyOnWriteArrayList[A]] = semigroup[CopyOnWriteArrayList[A]]((a, b) => {
    val k = a.clone.asInstanceOf[CopyOnWriteArrayList[A]]
    k addAll b
    k
  })

  implicit def JavaLinkedBlockingQueueSemigroup[A]: Semigroup[LinkedBlockingQueue[A]] = semigroup[LinkedBlockingQueue[A]]((a, b) => {
    val k = new LinkedBlockingQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaSynchronousQueueSemigroup[A]: Semigroup[SynchronousQueue[A]] = semigroup[SynchronousQueue[A]]((a, b) => {
    val k = new SynchronousQueue[A]
    k addAll a
    k addAll b
    k
  })
}
