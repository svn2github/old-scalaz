package scalaz

trait FoldRight[-F[_]] {
  def foldRight[A, B](t: F[A], b: B, f: (A, => B) => B): B
}

object FoldRight {
  implicit val IdentityFoldRight = new FoldRight[Identity] {
    def foldRight[A, B](t: Identity[A], b: B, f: (A, => B) => B) = f(t.value, b)
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

  implicit val ArrayFoldRight = new FoldRight[Array] {
    def foldRight[A, B](t: Array[A], b: B, f: (A, => B) => B) = t.foldRight(b)(f(_, _))
  }

  implicit def EitherLeftFoldRight[X] = new FoldRight[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def foldRight[A, B](e: Either.LeftProjection[A, X], b: B, f: (A, => B) => B) = OptionFoldRight.foldRight(e.toOption, b, f)
  }

  implicit def EitherRightFoldRight[X] = new FoldRight[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def foldRight[A, B](e: Either.RightProjection[X, A], b: B, f: (A, => B) => B) = OptionFoldRight.foldRight(e.toOption, b, f)
  }

  implicit val IterableFoldRight = new FoldRight[Iterable] {
    def foldRight[A, B](t: Iterable[A], b: B, f: (A, => B) => B): B = t.foldRight(b)(f(_, _))
  }

  implicit def JavaListFoldRight[A] = new FoldRight[java.util.List] {
    def foldRight[A, B](t: java.util.List[A], b: B, f: (A, => B) => B) = {
      var x = b
      val i = t.listIterator

      while(i.hasPrevious) {
        val n = i.previous
        x = f(n, x)
      }

      x
    }
  }
}
