package scalaz

trait Empty[+E[_]] {
  def empty[A]: E[A]
}

object Empty {
  import Scalaz._

  implicit lazy val ZipStreamEmpty = new Empty[ZipStream] {
    def empty[A] = emptyZipStream
  }

  implicit lazy val ListEmpty: Empty[List] = new Empty[List] {
    def empty[A] = Nil
  }

  implicit lazy val StreamEmpty: Empty[Stream] = new Empty[Stream] {
    def empty[A] = Stream.empty
  }

  implicit lazy val OptionEmpty: Empty[Option] = new Empty[Option] {
    def empty[A] = None
  }

  implicit lazy val GenericArrayEmpty: Empty[GArray] = new Empty[GArray] {
    def empty[A] = new GArray(0)
  }

  implicit def EitherLeftEmpty[X](implicit z: Zero[X]): Empty[PartialApply1Of2[Either.LeftProjection, X]#Flip] = new Empty[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def empty[A] = Right(z.zero).left
  }

  implicit def EitherRightEmpty[X](implicit z: Zero[X]): Empty[PartialApply1Of2[Either.RightProjection, X]#Apply] = new Empty[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def empty[A] = Left(z.zero).right
  }

  import java.util._
  import java.util.concurrent._

  implicit lazy val JavaArrayListEmpty: Empty[ArrayList] = new Empty[ArrayList] {
    def empty[A] = new ArrayList[A]
  }

  implicit lazy val JavaHashSetEmpty: Empty[HashSet] = new Empty[HashSet] {
    def empty[A] = new HashSet[A]
  }

  implicit lazy val JavaLinkedHashSetEmpty: Empty[LinkedHashSet] = new Empty[LinkedHashSet] {
    def empty[A] = new LinkedHashSet[A]
  }

  implicit lazy val JavaLinkedListEmpty: Empty[LinkedList] = new Empty[LinkedList] {
    def empty[A] = new LinkedList[A]
  }

  implicit lazy val JavaPriorityQueueEmpty: Empty[PriorityQueue] = new Empty[PriorityQueue] {
    def empty[A] = new PriorityQueue[A]
  }

  implicit lazy val JavaStackEmpty: Empty[Stack] = new Empty[Stack] {
    def empty[A] = new Stack[A]
  }

  implicit lazy val JavaTreeSetEmpty: Empty[TreeSet] = new Empty[TreeSet] {
    def empty[A] = new TreeSet[A]
  }

  implicit lazy val JavaVectorEmpty: Empty[Vector] = new Empty[Vector] {
    def empty[A] = new Vector[A]
  }

  implicit lazy val JavaArrayBlockingQueueEmpty: Empty[ArrayBlockingQueue] = new Empty[ArrayBlockingQueue] {
    def empty[A] = new ArrayBlockingQueue[A](0)
  }

  implicit lazy val JavaConcurrentLinkedQueueEmpty: Empty[ConcurrentLinkedQueue] = new Empty[ConcurrentLinkedQueue] {
    def empty[A] = new ConcurrentLinkedQueue[A]
  }

  implicit lazy val JavaCopyOnWriteArrayListEmpty: Empty[CopyOnWriteArrayList] = new Empty[CopyOnWriteArrayList] {
    def empty[A] = new CopyOnWriteArrayList[A]
  }

  implicit lazy val JavaCopyOnWriteArraySetEmpty: Empty[CopyOnWriteArraySet] = new Empty[CopyOnWriteArraySet] {
    def empty[A] = new CopyOnWriteArraySet[A]
  }

  implicit lazy val JavaLinkedBlockingQueueEmpty: Empty[LinkedBlockingQueue] = new Empty[LinkedBlockingQueue] {
    def empty[A] = new LinkedBlockingQueue[A]
  }

  implicit lazy val JavaSynchronousQueueEmpty: Empty[SynchronousQueue] = new Empty[SynchronousQueue] {
    def empty[A] = new SynchronousQueue[A]
  }
}
