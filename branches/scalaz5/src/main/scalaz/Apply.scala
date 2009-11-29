package scalaz

trait Apply[Z[_]] {
  def apply[A, B](f: Z[A => B], a: Z[A]): Z[B]
}

trait Applys {
  def FunctorBindApply[Z[_]](implicit t: Functor[Z], b: Bind[Z]) = new Apply[Z] {
    def apply[A, B](f: Z[A => B], a: Z[A]): Z[B] = {
      b.bind(f, (g: A => B) => t.fmap(a, g(_: A)))
    }
  }  
}

object Apply {
  import Scalaz._

  implicit val IdentityApply: Apply[Identity] = FunctorBindApply[Identity]

  implicit val NonEmptyListApply: Apply[NonEmptyList] = FunctorBindApply[NonEmptyList]

  implicit def StateApply[S]: Apply[PartialApply1Of2[State, S]#Apply] = FunctorBindApply[PartialApply1Of2[State, S]#Apply]

  implicit val Tuple1Apply: Apply[Tuple1] = FunctorBindApply[Tuple1]

  implicit def Tuple2Apply[R](implicit sr: Semigroup[R]): Apply[PartialApply1Of2[Tuple2, R]#Apply] = FunctorBindApply[PartialApply1Of2[Tuple2, R]#Apply]

  implicit def Tuple3Apply[R, S](implicit sr: Semigroup[R], ss: Semigroup[S]): Apply[PartialApply2Of3[Tuple3, R, S]#Apply] = FunctorBindApply[PartialApply2Of3[Tuple3, R, S]#Apply]

  implicit def Tuple4Apply[R, S, T](implicit sr: Semigroup[R], ss: Semigroup[S], st: Semigroup[T]): Apply[PartialApply3Of4[Tuple4, R, S, T]#Apply] = FunctorBindApply[PartialApply3Of4[Tuple4, R, S, T]#Apply]

  implicit def Tuple5Apply[R, S, T, U](implicit sr: Semigroup[R], ss: Semigroup[S], st: Semigroup[T], su: Semigroup[U]): Apply[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] = FunctorBindApply[PartialApply4Of5[Tuple5, R, S, T, U]#Apply]

  implicit def Tuple6Apply[R, S, T, U, V](implicit sr: Semigroup[R], ss: Semigroup[S], st: Semigroup[T], su: Semigroup[U], sv: Semigroup[V]): Apply[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] = FunctorBindApply[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply]

  implicit def Tuple7Apply[R, S, T, U, V, W](implicit sr: Semigroup[R], ss: Semigroup[S], st: Semigroup[T], su: Semigroup[U], sv: Semigroup[V], sw: Semigroup[W]): Apply[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] = FunctorBindApply[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply]

  implicit val Function0Apply: Apply[Function0] = FunctorBindApply[Function0]

  implicit def Function1Apply[R]: Apply[PartialApply1Of2[Function1, R]#Apply] = FunctorBindApply[PartialApply1Of2[Function1, R]#Apply]

  implicit def Function2Apply[R, S]: Apply[PartialApply2Of3[Function2, R, S]#Apply] = FunctorBindApply[PartialApply2Of3[Function2, R, S]#Apply]

  implicit def Function3Apply[R, S, T]: Apply[PartialApply3Of4[Function3, R, S, T]#Apply] = FunctorBindApply[PartialApply3Of4[Function3, R, S, T]#Apply]

  implicit def Function4Apply[R, S, T, U]: Apply[PartialApply4Of5[Function4, R, S, T, U]#Apply] = FunctorBindApply[PartialApply4Of5[Function4, R, S, T, U]#Apply]

  implicit def Function5Apply[R, S, T, U, V]: Apply[PartialApply5Of6[Function5, R, S, T, U, V]#Apply] = FunctorBindApply[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]

  implicit def Function6Apply[R, S, T, U, V, W]: Apply[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply] = FunctorBindApply[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]

  implicit val ListApply: Apply[List] = FunctorBindApply[List]

  implicit val StreamApply: Apply[Stream] = FunctorBindApply[Stream]

  implicit val OptionApply: Apply[Option] = FunctorBindApply[Option]

  implicit val GenericArrayApply: Apply[GArray] = FunctorBindApply[GArray]

  implicit def EitherLeftApply[X]: Apply[PartialApply1Of2[Either.LeftProjection, X]#Flip] = FunctorBindApply[PartialApply1Of2[Either.LeftProjection, X]#Flip]

  implicit def EitherRightApply[X]: Apply[PartialApply1Of2[Either.RightProjection, X]#Apply] = FunctorBindApply[PartialApply1Of2[Either.RightProjection, X]#Apply]

  implicit def ValidationApply[X](implicit s: Semigroup[X]): Apply[PartialApply1Of2[Validation, X]#Apply] = new Apply[PartialApply1Of2[Validation, X]#Apply] {
    def apply[A, B](f: Validation[X, A => B], a: Validation[X, A]) = (f, a) match {
      case (Success(f), Success(a)) => success(f(a))
      case (Success(_), Failure(e)) => failure(e)
      case (Failure(e), Success(_)) => failure(e)
      case (Failure(e1), Failure(e2)) => failure(e1 ⊹ e2)
    }
  }

  implicit def ValidationFailureApply[X]: Apply[PartialApply1Of2[FailProjection, X]#Flip] = new Apply[PartialApply1Of2[FailProjection, X]#Flip] {
    def apply[A, B](f: FailProjection[A => B, X], a: FailProjection[A, X]) = ((f.validation, a.validation) match {
      case (Success(x1), Success(_)) => success(x1)
      case (Success(x1), Failure(_)) => success(x1)
      case (Failure(_), Success(x2)) => success(x2)
      case (Failure(f), Failure(e)) => failure(f(e))
    }).fail
  }

  /* todo
  implicit val ZipperApply: Apply[Zipper] = new Apply[Zipper] {
    def apply[A, B](f: Zipper[A => B], a: Zipper[A]): Zipper[B] =
      Zipper.zipper((a.lefts |!|) <*> (f.lefts |!|),
        (f.focus)(a.focus),
        (a.rights |!|) <*> (f.rights |!|))
  }
      */
  implicit val ZipStreamApply: Apply[ZipStream] = new Apply[ZipStream] {
    def apply[A, B](f: ZipStream[A => B], a: ZipStream[A]): ZipStream[B] = {
      val ff = f.value
      val aa = a.value
      (if (ff.isEmpty || aa.isEmpty) Stream.empty
      else Stream.cons((ff.head)(aa.head), apply(ff.tail |!|, aa.tail |!|))) |!|
    }
  }
       /*
  val ZipTreeApply: Apply[Tree] = new Apply[Tree] {
    def apply[A, B](f: Tree[A => B], a: Tree[A]): Tree[B] = {
      Tree.node((f.rootLabel)(a.rootLabel), (a.subForest |!|) <*> (f.subForest.map((apply(_: Tree[A => B], _: Tree[A])).curry) |!|))
    }
  }

  implicit val TreeApply = FunctorBindApply[Tree]

  import concurrent.Promise
  implicit val PromiseApply = FunctorBindApply[Promise]
  */
  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListApply: Apply[ArrayList] = FunctorBindApply[ArrayList]

  implicit val JavaLinkedListApply: Apply[LinkedList] = FunctorBindApply[LinkedList]

  implicit val JavaPriorityQueueApply: Apply[PriorityQueue] = FunctorBindApply[PriorityQueue]

  implicit val JavaStackApply: Apply[Stack] = FunctorBindApply[Stack]

  implicit val JavaVectorApply: Apply[Vector] = FunctorBindApply[Vector]

  implicit val JavaArrayBlockingQueueApply: Apply[ArrayBlockingQueue] = FunctorBindApply[ArrayBlockingQueue]

  implicit val JavaConcurrentLinkedQueueApply: Apply[ConcurrentLinkedQueue] = FunctorBindApply[ConcurrentLinkedQueue]

  implicit val JavaCopyOnWriteArrayListApply: Apply[CopyOnWriteArrayList] = FunctorBindApply[CopyOnWriteArrayList]

  implicit val JavaLinkedBlockingQueueApply: Apply[LinkedBlockingQueue] = FunctorBindApply[LinkedBlockingQueue]

  implicit val JavaSynchronousQueueApply: Apply[SynchronousQueue] = FunctorBindApply[SynchronousQueue]
}
