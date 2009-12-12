package scalaz

trait Pure[+P[_]] {
  def pure[A](a: => A): P[A]
}

object Pure {
  import Scalaz._

  implicit def IdentityPure: Pure[Identity] = new Pure[Identity] {
    def pure[A](a: => A) = a
  }

  implicit def NonEmptyListPure: Pure[NonEmptyList] = new Pure[NonEmptyList] {
    def pure[A](a: => A) = a.wrapNel
  }

  implicit def StatePure[S]: Pure[PartialApply1Of2[State, S]#Apply] = new Pure[PartialApply1Of2[State, S]#Apply] {
    def pure[A](a: => A) = a.state[S]
  }

  implicit def Tuple1Pure = new Pure[Tuple1] {
    def pure[A](a: => A) = Tuple1(a)
  }

  implicit def Tuple2Pure[R: Zero]: Pure[PartialApply1Of2[Tuple2, R]#Apply] = new Pure[PartialApply1Of2[Tuple2, R]#Apply] {
    def pure[A](a: => A) = (∅, a)
  }

  implicit def Tuple3Pure[R: Zero, S: Zero]: Pure[PartialApply2Of3[Tuple3, R, S]#Apply] = new Pure[PartialApply2Of3[Tuple3, R, S]#Apply] {
    def pure[A](a: => A) = (∅, ∅, a)
  }

  implicit def Tuple4Pure[R: Zero, S: Zero, T: Zero]: Pure[PartialApply3Of4[Tuple4, R, S, T]#Apply] = new Pure[PartialApply3Of4[Tuple4, R, S, T]#Apply] {
    def pure[A](a: => A) = (∅, ∅, ∅, a)
  }

  implicit def Tuple5Pure[R: Zero, S: Zero, T: Zero, U: Zero]: Pure[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] = new Pure[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] {
    def pure[A](a: => A) = (∅, ∅, ∅, ∅, a)
  }

  implicit def Tuple6Pure[R: Zero, S: Zero, T: Zero, U: Zero, V: Zero]: Pure[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] = new Pure[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] {
    def pure[A](a: => A) = (∅, ∅, ∅, ∅, ∅, a)
  }

  implicit def Tuple7Pure[R: Zero, S: Zero, T: Zero, U: Zero, V: Zero, W: Zero]: Pure[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] = new Pure[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] {
    def pure[A](a: => A) = (∅, ∅, ∅, ∅, ∅, ∅, a)
  }

  implicit def Function0Pure: Pure[Function0] = new Pure[Function0] {
    def pure[A](a: => A) = new Function0[A] {
      def apply = a
    }
  }

  implicit def Function1Pure[R]: Pure[PartialApply1Of2[Function1, R]#Apply] = new Pure[PartialApply1Of2[Function1, R]#Apply] {
    def pure[A](a: => A) = (_: R) => a
  }

  implicit def Function2Pure[R, S]: Pure[PartialApply2Of3[Function2, R, S]#Apply] = new Pure[PartialApply2Of3[Function2, R, S]#Apply] {
    def pure[A](a: => A) = (_: R, _: S) => a
  }

  implicit def Function3Pure[R, S, T]: Pure[PartialApply3Of4[Function3, R, S, T]#Apply] = new Pure[PartialApply3Of4[Function3, R, S, T]#Apply] {
    def pure[A](a: => A) = (_: R, _: S, _: T) => a
  }

  implicit def Function4Pure[R, S, T, U]: Pure[PartialApply4Of5[Function4, R, S, T, U]#Apply] = new Pure[PartialApply4Of5[Function4, R, S, T, U]#Apply] {
    def pure[A](a: => A) = (_: R, _: S, _: T, _: U) => a
  }

  implicit def Function5Pure[R, S, T, U, V]: Pure[PartialApply5Of6[Function5, R, S, T, U, V]#Apply] = new Pure[PartialApply5Of6[Function5, R, S, T, U, V]#Apply] {
    def pure[A](a: => A) = (_: R, _: S, _: T, _: U, _: V) => a
  }

  implicit def Function6Pure[R, S, T, U, V, W]: Pure[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply] = new Pure[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply] {
    def pure[A](a: => A) = (_: R, _: S, _: T, _: U, _: V, _: W) => a
  }

  implicit def ListPure: Pure[List] = new Pure[List] {
    def pure[A](a: => A) = List(a)
  }

  implicit def StreamPure: Pure[Stream] = new Pure[Stream] {
    def pure[A](a: => A) = Stream(a)
  }

  implicit def OptionPure: Pure[Option] = new Pure[Option] {
    def pure[A](a: => A) = Some(a)
  }

  implicit def GenericArrayPure: Pure[GArray] = new Pure[GArray] {
    def pure[A](a: => A) = {
      val t = new GArray[A](1)
      t(0) = a
      t
    }
  }

  implicit def EitherLeftPure[X]: Pure[PartialApply1Of2[Either.LeftProjection, X]#Flip] = new Pure[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def pure[A](a: => A) = Left(a).left
  }

  implicit def EitherRightPure[X]: Pure[PartialApply1Of2[Either.RightProjection, X]#Apply] = new Pure[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def pure[A](a: => A) = Right(a).right
  }

  implicit def ResponderPure: Pure[Responder] = new Pure[Responder] {
    def pure[A](a: => A) = new Responder[A] {
      def respond(k: A => Unit) = k(a)
    }
  }

  import java.util.concurrent.Callable

  implicit def CallablePure: Pure[Callable] = new Pure[Callable] {
    def pure[A](a: => A) = new Callable[A] {
      def call = a
    }
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryPure[X: Zero]: Pure[PartialApply1Of2[Entry, X]#Apply] = new Pure[PartialApply1Of2[Entry, X]#Apply] {
    def pure[A](a: => A) = new SimpleImmutableEntry(∅, a)
  }

  implicit def ValidationPure[X]: Pure[PartialApply1Of2[Validation, X]#Apply] = new Pure[PartialApply1Of2[Validation, X]#Apply] {
    def pure[A](a: => A) = a.success
  }

  implicit def ValidationFailurePure[X]: Pure[PartialApply1Of2[FailProjection, X]#Flip] = new Pure[PartialApply1Of2[FailProjection, X]#Flip] {
    def pure[A](a: => A) = Failure(a).fail
  }

  implicit def ZipperPure = new Pure[Zipper] {
    def pure[A](a: => A) = a.zipper
  }

  implicit def ZipStreamPure: Pure[ZipStream] = new Pure[ZipStream] {
    def pure[A](a: => A) = zip(Stream(a))
  }

  implicit def EndoPure: Pure[Endo] = new Pure[Endo] {
    def pure[A](a: => A) = constantEndo(a)
  }

  implicit def TreePure: Pure[Tree] = new Pure[Tree] {
    def pure[A](a: => A) = node(a, Stream.Empty)
  }

  implicit def TreeLocPure: Pure[TreeLoc] = new Pure[TreeLoc] {
    def pure[A](a: => A) = TreePure.pure(a).loc
  }

  import concurrent._
  implicit def PromisePure(implicit s: Strategy[Unit]): Pure[Promise] = new Pure[Promise] {
    def pure[A](a: => A) = promise(a)
  }

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListPure: Pure[ArrayList] = new Pure[ArrayList] {
    def pure[A](a: => A) = {
      val k = new ArrayList[A]
      k add a
      k
    }
  }

  implicit def JavaHashSetPure: Pure[HashSet] = new Pure[HashSet] {
    def pure[A](a: => A) = {
      val k = new HashSet[A]
      k add a
      k
    }
  }

  implicit def JavaLinkedHashSetPure: Pure[LinkedHashSet] = new Pure[LinkedHashSet] {
    def pure[A](a: => A) = {
      val k = new LinkedHashSet[A]
      k add a
      k
    }
  }

  implicit def JavaLinkedListPure: Pure[LinkedList] = new Pure[LinkedList] {
    def pure[A](a: => A) = {
      val k = new LinkedList[A]
      k add a
      k
    }
  }

  implicit def JavaPriorityQueuePure: Pure[PriorityQueue] = new Pure[PriorityQueue] {
    def pure[A](a: => A) = {
      val k = new PriorityQueue[A]
      k add a
      k
    }
  }

  implicit def JavaStackPure: Pure[Stack] = new Pure[Stack] {
    def pure[A](a: => A) = {
      val k = new Stack[A]
      k add a
      k
    }
  }

  implicit def JavaTreeSetPure: Pure[TreeSet] = new Pure[TreeSet] {
    def pure[A](a: => A) = {
      val k = new TreeSet[A]
      k add a
      k
    }
  }

  implicit def JavaVectorPure: Pure[Vector] = new Pure[Vector] {
    def pure[A](a: => A) = {
      val k = new Vector[A]
      k add a
      k
    }
  }

  implicit def JavaArrayBlockingQueuePure: Pure[ArrayBlockingQueue] = new Pure[ArrayBlockingQueue] {
    def pure[A](a: => A) = {
      val k = new ArrayBlockingQueue[A](0)
      k add a
      k
    }
  }

  implicit def JavaConcurrentLinkedQueuePure: Pure[ConcurrentLinkedQueue] = new Pure[ConcurrentLinkedQueue] {
    def pure[A](a: => A) = {
      val k = new ConcurrentLinkedQueue[A]
      k add a
      k
    }
  }

  implicit def JavaCopyOnWriteArrayListPure: Pure[CopyOnWriteArrayList] = new Pure[CopyOnWriteArrayList] {
    def pure[A](a: => A) = {
      val k = new CopyOnWriteArrayList[A]
      k add a
      k
    }
  }

  implicit def JavaCopyOnWriteArraySetPure: Pure[CopyOnWriteArraySet] = new Pure[CopyOnWriteArraySet] {
    def pure[A](a: => A) = {
      val k = new CopyOnWriteArraySet[A]
      k add a
      k
    }
  }

  implicit def JavaLinkedBlockingQueuePure: Pure[LinkedBlockingQueue] = new Pure[LinkedBlockingQueue] {
    def pure[A](a: => A) = {
      val k = new LinkedBlockingQueue[A]
      k add a
      k
    }
  }

  implicit def JavaSynchronousQueuePure: Pure[SynchronousQueue] = new Pure[SynchronousQueue] {
    def pure[A](a: => A) = {
      val k = new SynchronousQueue[A]
      k add a
      k
    }
  }
}
