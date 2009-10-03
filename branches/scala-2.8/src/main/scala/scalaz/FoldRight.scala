package scalaz

trait FoldRight[-F[_]] {
  def foldRight[A, B](t: F[A], b: B, f: (A, => B) => B): B
}

object FoldRight {
  import Scalaz._

  implicit val IdentityFoldRight = new FoldRight[Identity] {
    def foldRight[A, B](t: Identity[A], b: B, f: (A, => B) => B) = f(t.value, b)
  }
  
  implicit val ListFoldRight = new FoldRight[List] {
    def foldRight[A, B](t: List[A], b: B, f: (A, => B) => B) = IterableFoldRight.foldRight(t, b, f)
  }

  implicit val NonEmptyListFoldRight = new FoldRight[NonEmptyList] {
    def foldRight[A, B](t: NonEmptyList[A], b: B, f: (A, => B) => B) = IterableFoldRight.foldRight(t.list, b, f)
  }

  implicit val StateFoldRight = new FoldRight[PartialApply1Of2[State, Unit]#Apply] {
    def foldRight[A, B](t: State[Unit, A], b: B, f: (A, => B) => B) = f(t(())._2, b)
  }

  implicit val Tuple1FoldRight = new FoldRight[Tuple1] {
    def foldRight[A, B](t: Tuple1[A], b: B, f: (A, => B) => B) = f(t._1, b)
  }

  implicit val Function0FoldRight = new FoldRight[Function0] {
    def foldRight[A, B](t: Function0[A], b: B, f: (A, => B) => B) = f(t.apply, b)
  }

  implicit val OptionFoldRight = new FoldRight[Option] {
    def foldRight[A, B](t: Option[A], b: B, f: (A, => B) => B) = t match {
      case Some(a) => f(a, b)
      case None => b
    }
  }


  implicit val TreeFoldRight: FoldRight[Tree] = new FoldRight[Tree] {
    def foldRight[A, B](t: Tree[A], b: B, f: (A, => B) => B): B = {
      import Zero._
      import Semigroup._
      import Monoid._
      import Endo._
      val m: Monoid[Endo[B]] = monoid(EndoSemigroup[B], EndoZero[B])
      t.foldMap((a) => EndoTo(f.curry(a)(_: B)))(m)(b)
    }
  }

  implicit val ZipperFoldRight: FoldRight[Zipper] = new FoldRight[Zipper] {
    def foldRight[A, B](t: Zipper[A], b: B, f: (A, => B) => B): B =
      t.lefts.foldLeft(Stream.cons(t.focus, t.rights).foldRight(b)(f(_, _)))((f.flip)(_, _))
  }

  implicit val ZipStreamFoldRight: FoldRight[ZipStream] = new FoldRight[ZipStream] {
    def foldRight[A, B](t: ZipStream[A], b: B, f: (A, => B) => B): B = StreamFoldRight.foldRight(t.value, b, f)     
  }

  implicit object ArrayFoldRight extends FoldRight[Array] {
    def foldRight[A, B](t: Array[A], b: B, f: (A, => B) => B) = t.foldRight(b)(f(_, _))
  }

  implicit def EitherLeftFoldRight[X] = new FoldRight[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def foldRight[A, B](e: Either.LeftProjection[A, X], b: B, f: (A, => B) => B) = OptionFoldRight.foldRight(e.toOption, b, f)
  }

  implicit def EitherRightFoldRight[X] = new FoldRight[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def foldRight[A, B](e: Either.RightProjection[X, A], b: B, f: (A, => B) => B) = OptionFoldRight.foldRight(e.toOption, b, f)
  }

  implicit def ValidationFoldRight[X] = new FoldRight[PartialApply1Of2[Validation, X]#Apply] {
    def foldRight[A, B](e: Validation[X, A], b: B, f: (A, => B) => B) = e match {
      case Success(a) => f(a, b)
      case Failure(_) => b
    }
  }

  implicit def ValidationFailureFoldRight[X] = new FoldRight[PartialApply1Of2[Validation.FailureProjection, X]#Flip] {
    def foldRight[A, B](e: Validation.FailureProjection[A, X], b: B, f: (A, => B) => B) = e.validation match {
      case Success(_) => b
      case Failure(e) => f(e, b)
    }
  }

  implicit val StreamFoldRight = new FoldRight[Stream] {
    def foldRight[A, B](t: Stream[A], b: B, f: (A, => B) => B): B = if(t.isEmpty) b else f(t.head, foldRight(t.tail, b, f))
  }

  implicit val IterableFoldRight = new FoldRight[Iterable] {
    def foldRight[A, B](t: Iterable[A], b: B, f: (A, => B) => B): B = t.foldRight(b)(f(_, _))
  }

  implicit def JavaIterableFoldRight[A] = new FoldRight[java.lang.Iterable] {
    def foldRight[A, B](t: java.lang.Iterable[A], b: B, f: (A, => B) => B) = {
      val i = scala.collection.JavaConversions.JIterableWrapper(t)
      IterableFoldRight.foldRight(i, b, f)
    }
  }

  import org.scalacheck.Constraint

  implicit val ConstraintFoldRight = new FoldRight[Constraint] {
import collection.JavaConversions.JIterableWrapper

def foldRight[A, B](t: Constraint[A], b: B, f: (A, => B) => B) = f(t.unbox, b)
  }
}
