package scalaz

/**
 * A Semigroup in type S must satisfy two laws:
 * <ol>
 * <li>
 * Closure: ∀ a, b in S, append(a, b) is also in S. This is enforced by the type system.
 * </li>
 * <li>
 * Associativity: ∀ a, b and c in S, the equation append(append(a, b), c) = append(a, append(b , c)) holds.
 * </li>
 * </ol>
 * @see scalaz.Identity#⊹
 */
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
  import xml.NodeSeq

  implicit def DigitSemigroup: Semigroup[Digit] = semigroup((a, b) => a.toInt + b.toInt)

  implicit def OrderingSemigroup: Semigroup[Ordering] = semigroup {
    case (EQ, a) => a
    case (LT, _) => LT
    case (GT, _) => GT
  }

  implicit def UnitSemigroup: Semigroup[Unit] = semigroup((_, _) => ())

  implicit def StringSemigroup: Semigroup[String] = semigroup(_ + _)

  implicit def IntSemigroup: Semigroup[Int] = semigroup(_ + _)

  implicit def IntMultiplicationSemigroup: Semigroup[IntMultiplication] = semigroup(_ * _ ∏)

  implicit def BooleanConjunctionSemigroup: Semigroup[BooleanConjunction] = semigroup(_ && _ |∧|)

  implicit def BooleanSemigroup: Semigroup[Boolean] = semigroup((a, b) => (a || b))

  implicit def CharSemigroup: Semigroup[Char] = semigroup((a, b) => (a + b).toChar)

  implicit def CharMultiplicationSemigroup: Semigroup[CharMultiplication] = semigroup((a, b) => (a * b).toChar ∏)

  implicit def ByteSemigroup: Semigroup[Byte] = semigroup((a, b) => (a + b).toByte)

  implicit def ByteMultiplicationSemigroup: Semigroup[ByteMultiplication] = semigroup((a, b) => (a * b).toByte ∏)

  implicit def LongSemigroup: Semigroup[Long] = semigroup((a, b) => (a + b).toLong)

  implicit def LongMultiplicationSemigroup: Semigroup[LongMultiplication] = semigroup((a, b) => (a * b).toLong ∏)

  implicit def ShortSemigroup: Semigroup[Short] = semigroup((a, b) => (a + b).toShort)

  implicit def ShortMultiplicationSemigroup: Semigroup[ShortMultiplication] = semigroup((a, b) => (a * b).toShort ∏)

  implicit def FloatSemigroup: Semigroup[Float] = semigroup((a, b) => (a + b).toFloat)

  implicit def DoubleSemigroup: Semigroup[Double] = semigroup((a, b) => (a + b).toDouble)

  implicit def BigIntegerSemigroup: Semigroup[java.math.BigInteger] = semigroup(_ add _)

  implicit def BigIntegerMutliplicationSemigroup: Semigroup[BigIntegerMultiplication] = semigroup(_.value multiply _.value ∏)

  implicit def BigIntSemigroup: Semigroup[BigInt] = semigroup(_ + _)

  implicit def BigIntMultiplicationSemigroup: Semigroup[BigIntMultiplication] = semigroup(_.value * _.value ∏)

  implicit def NodeSeqSemigroup: Semigroup[NodeSeq] = semigroup(_ ++ _)

  implicit def NonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] = semigroup(_.list <::: _)
  
  implicit def ZipStreamSemigroup[A]: Semigroup[ZipStream[A]] = semigroup(_.value append _.value ʐ)

  implicit def ListSemigroup[A]: Semigroup[List[A]] = semigroup(_ ::: _)

  implicit def StreamSemigroup[A]: Semigroup[Stream[A]] = semigroup(_ append _)

  implicit def OptionSemigroup[A : Semigroup]: Semigroup[Option[A]] = semigroup((a, b) => { (a,b) match {
    case (Some(va), Some(vb)) => Some(va ⊹ vb)
    case (Some(va), None) => Some(va)
    case (None, Some(vb)) => Some(vb)
    case (None, None) => None
  }})

  implicit def FirstSemigroup[A]: Semigroup[FirstOption[A]] = semigroup((a, b) => a orElse b)

  implicit def LastSemigroup[A]: Semigroup[LastOption[A]] = semigroup((a, b) => b orElse a)

  implicit def ArraySemigroup[A: Manifest]: Semigroup[Array[A]] = semigroup(Array.concat(_, _))

  implicit def GenericArraySemigroup[A]: Semigroup[GArray[A]] = semigroup(_ ++ _)

  implicit def EitherLeftSemigroup[A, B]: Semigroup[Either.LeftProjection[A, B]] = semigroup((a, b) => if (a.e.isLeft) a else b)

  implicit def EitherRightSemigroup[A, B]: Semigroup[Either.RightProjection[B, A]] = semigroup((a, b) => if (a.e.isRight) a else b)

  implicit def Function1ABSemigroup[A, B: Semigroup]: Semigroup[A => B] = semigroup((a1, a2) => a => a1(a) ⊹ a2.apply(a))

  implicit def EndoSemigroup[A]: Semigroup[Endo[A]] = semigroup((x, y) => ((a: A) => x(y.apply(a))))

  implicit def DualSemigroup[A: Semigroup]: Semigroup[Dual[A]] =
    semigroup((x, y) => y.value ⊹ x.value)

  implicit def SemigroupKleisliSemigroup[M[_],A,B](implicit ss: Semigroup[M[B]]): Semigroup[Kleisli[M,A,B]] = semigroup((k1, k2) => ☆((a : A) => k1(a) ⊹ k2.apply(a)))

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
