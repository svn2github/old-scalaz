package scalaz

trait Index[-I[_]] {
  def index[A](a: I[A], i: Int): Option[A]
}

object Index {
  import Scalaz._
  
  implicit lazy val IdentityIndex: Index[Identity] = new Index[Identity] {
    def index[A](a: Identity[A], i: Int) = if(i == 0) Some(a.value) else None
  }

  implicit def NonEmptyListIndex[A]: Index[NonEmptyList] = new Index[NonEmptyList] {
    def index[A](a: NonEmptyList[A], i: Int) = if(i == 0) Some(a.head) else IterableIndex.index(a.tail, i - 1)
  }

  implicit lazy val ZipStreamIndex: Index[ZipStream] = new Index[ZipStream] {
    def index[A](a: ZipStream[A], i: Int) = IterableIndex.index(a.value, i)
  }

  implicit lazy val Tuple1Index: Index[Tuple1] = new Index[Tuple1] {
    def index[A](a: Tuple1[A], i: Int) = if(i == 0) Some(a._1) else None
  }

  implicit lazy val Function0Index: Index[Function0] = new Index[Function0] {
    def index[A](a: Function0[A], i: Int) = if(i == 0) Some(a.apply) else None
  }

  implicit lazy val OptionIndex: Index[Option] = new Index[Option] {
    def index[A](a: Option[A], i: Int) = a filter (_ => i == 0)
  }

  implicit lazy val GenericArrayIndex: Index[GArray] = new Index[GArray] {
    def index[A](a: GArray[A], i: Int) = if(i >= 0 && i < a.length) Some(a(i)) else None
  }

  implicit def EitherLeftIndex[X]: Index[PartialApply1Of2[Either.LeftProjection, X]#Flip] = new Index[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def index[A](a: Either.LeftProjection[A, X], i: Int) = a.toOption filter (_ => i == 0)
  }

  implicit def EitherRightIndex[X]: Index[PartialApply1Of2[Either.RightProjection, X]#Apply] = new Index[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def index[A](a: Either.RightProjection[X, A], i: Int) = a.toOption filter (_ => i == 0)
  }

  implicit def ValidationIndex[X]: Index[PartialApply1Of2[Validation, X]#Apply] = new Index[PartialApply1Of2[Validation, X]#Apply] {
    def index[A](a: Validation[X, A], i: Int) = a.either.right.toOption filter (_ => i == 0)
  }

  implicit def ValidationFailureIndex[X]: Index[PartialApply1Of2[FailProjection, X]#Flip] = new Index[PartialApply1Of2[FailProjection, X]#Flip] {
    def index[A](a: FailProjection[A, X], i: Int) = a.validation.either.left.toOption filter (_ => i == 0)
  }

  implicit lazy val IterableIndex: Index[Iterable] = new Index[Iterable] {
    def index[A](a: Iterable[A], i: Int) = if(i < 0) None else {
      var n = 0
      var k: Option[A] = None
      val it = a.iterator
      while(it.hasNext && k.isEmpty) {
        val z = it.next
        if(n == i) k = Some(z)
        n = n + 1
      }

      k
    }
  }

  implicit lazy val JavaIterableIndex: Index[java.lang.Iterable] = new Index[java.lang.Iterable] {
    def index[A](a: java.lang.Iterable[A], i: Int) = if(i < 0) None else {
      var n = 0
      var k: Option[A] = None
      val it = a.iterator
      while(it.hasNext && k.isEmpty) {
        val z = it.next
        if(n == i) k = Some(z)
        n = n + 1
      }

      k
    }
  }
}
