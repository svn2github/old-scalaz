package scalaz

trait Plus[P[_]] {
  def plus[A](a1: P[A], a2: => P[A]): P[A]
}

object Plus {
  import Scalaz._

  implicit val NonEmptyListPlus: Plus[NonEmptyList] = new Plus[NonEmptyList] {
    def plus[A](a1: NonEmptyList[A], a2: => NonEmptyList[A]) = a1.list <::: a2
  }

  /* todo
  implicit val ZipStreamPlus: Plus[ZipStream] = new Plus[ZipStream] {
    def plus[A](a1: ZipStream[A], a2: => ZipStream[A]) = a1.value append a2.value |!|
  }
  */

  implicit val ListPlus: Plus[List] = new Plus[List] {
    def plus[A](a1: List[A], a2: => List[A]) = a1 ::: a2
  }

  implicit val StreamPlus: Plus[Stream] = new Plus[Stream] {
    def plus[A](a1: Stream[A], a2: => Stream[A]) = a1 append a2
  }

  implicit val OptionPlus: Plus[Option] = new Plus[Option] {
    def plus[A](a1: Option[A], a2: => Option[A]) = a1 orElse a2
  }

  /* todo
  implicit val ArrayPlus = new Plus[Array] {
    def plus[A](a1: Array[A], a2: => Array[A]) = a1 ++ a2
  }
  */

  implicit def EitherLeftPlus[X]: Plus[PartialApply1Of2[Either.LeftProjection, X]#Flip] = new Plus[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def plus[A](a1: Either.LeftProjection[A, X], a2: => Either.LeftProjection[A, X]) = a1.e match {
      case Left(_) => a1
      case Right(_) => a2.e match {
        case Left(_) => a2
        case Right(_) => a1
      }
    }
  }

  implicit def EitherRightPlus[X]: Plus[PartialApply1Of2[Either.RightProjection, X]#Apply] = new Plus[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def plus[A](a1: Either.RightProjection[X, A], a2: => Either.RightProjection[X, A]) = a2.e match {
      case Right(_) => a1
      case Left(_) => a2.e match {
        case Right(_) => a2
        case Left(_) => a1
      }
    }
  }

  implicit def ValidationPlus[X]: Plus[PartialApply1Of2[Validation, X]#Apply] = new Plus[PartialApply1Of2[Validation, X]#Apply] {
    def plus[A](a1: Validation[X, A], a2: => Validation[X, A]) = a1 match {
      case Success(_) => a1
      case Failure(_) => a2 match {
        case Success(_) => a2
        case Failure(_) => a1
      }
    }
  }

  implicit def ValidationFailurePlus[X]: Plus[PartialApply1Of2[FailProjection, X]#Flip] = new Plus[PartialApply1Of2[FailProjection, X]#Flip] {
    def plus[A](a1: FailProjection[A, X], a2: => FailProjection[A, X]) = a1.validation match {
      case Success(_) => a2.validation match {
        case Failure(_) => a2
        case Success(_) => a1
      }
      case Failure(_) => a1
    }
  }

  import java.util._
  import java.util.concurrent._

  implicit val ArrayListPlus: Plus[ArrayList] = new Plus[ArrayList] {
    def plus[A](a1: ArrayList[A], a2: => ArrayList[A]) = {
      val k = a1.clone.asInstanceOf[ArrayList[A]]
      k addAll a2
      k
    }
  }

  implicit val LinkedListPlus: Plus[LinkedList] = new Plus[LinkedList] {
    def plus[A](a1: LinkedList[A], a2: => LinkedList[A]) = {
      val k = a1.clone.asInstanceOf[LinkedList[A]]
      k addAll a2
      k
    }
  }

  implicit val PriorityQueuePlus: Plus[PriorityQueue] = new Plus[PriorityQueue] {
    def plus[A](a1: PriorityQueue[A], a2: => PriorityQueue[A]) = {
      val k = new PriorityQueue[A](a1)
      k addAll a2
      k
    }
  }

  implicit val StackPlus: Plus[Stack] = new Plus[Stack] {
    def plus[A](a1: Stack[A], a2: => Stack[A]) = {
      val k = a1.clone.asInstanceOf[Stack[A]]
      k addAll a2
      k
    }
  }

  implicit val VectorPlus: Plus[Vector] = new Plus[Vector] {
    def plus[A](a1: Vector[A], a2: => Vector[A]) = {
      val k = a1.clone.asInstanceOf[Vector[A]]
      k addAll a2
      k
    }
  }

  implicit val ArrayBlockingQueuePlus: Plus[ArrayBlockingQueue] = new Plus[ArrayBlockingQueue] {
    def plus[A](a1: ArrayBlockingQueue[A], a2: => ArrayBlockingQueue[A]) = {
      val k = new ArrayBlockingQueue[A](a1.remainingCapacity + a2.remainingCapacity)
      k addAll a1
      k addAll a2
      k
    }
  }

  implicit val ConcurrentLinkedQueuePlus: Plus[ConcurrentLinkedQueue] = new Plus[ConcurrentLinkedQueue] {
    def plus[A](a1: ConcurrentLinkedQueue[A], a2: => ConcurrentLinkedQueue[A]) = {
      val k = new ConcurrentLinkedQueue[A](a1)
      k addAll a2
      k
    }
  }

  implicit val CopyOnWriteArrayListPlus: Plus[CopyOnWriteArrayList] = new Plus[CopyOnWriteArrayList] {
    def plus[A](a1: CopyOnWriteArrayList[A], a2: => CopyOnWriteArrayList[A]) = {
      val k = a1.clone.asInstanceOf[CopyOnWriteArrayList[A]]
      k addAll a2
      k
    }
  }

  implicit val LinkedBlockingQueuePlus: Plus[LinkedBlockingQueue] = new Plus[LinkedBlockingQueue] {
    def plus[A](a1: LinkedBlockingQueue[A], a2: => LinkedBlockingQueue[A]) = {
      val k = new LinkedBlockingQueue[A](a1)
      k addAll a2
      k
    }
  }

  implicit val SynchronousQueuePlus: Plus[SynchronousQueue] = new Plus[SynchronousQueue] {
    def plus[A](a1: SynchronousQueue[A], a2: => SynchronousQueue[A]) = {
      val k = new SynchronousQueue[A]
      k addAll a1
      k addAll a2
      k
    }
  }
}
