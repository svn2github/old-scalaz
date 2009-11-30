package scalaz

trait Each[-E[_]] {
  def each[A](e: E[A], f: A => Unit): Unit
}

object Each {
  import Scalaz._

  implicit def IdentityEach: Each[Identity] = new Each[Identity] {
    def each[A](e: Identity[A], f: A => Unit) = f(e.value)
  }

  implicit def NonEmptyListEach[A]: Each[NonEmptyList] = new Each[NonEmptyList] {
    def each[A](e: NonEmptyList[A], f: A => Unit) = e.list foreach f
  }

  implicit def StateEach: Each[PartialApply1Of2[State, Unit]#Apply] = new Each[PartialApply1Of2[State, Unit]#Apply] {
    def each[A](e: State[Unit, A], f: A => Unit) = f(e(())._2)
  }

  implicit def ZipStreamEach = new Each[ZipStream] {
    def each[A](e: ZipStream[A], f: A => Unit) = e.value foreach f
  }

  implicit def Tuple1Each: Each[Tuple1] = new Each[Tuple1] {
    def each[A](e: Tuple1[A], f: A => Unit) = f(e._1)
  }

  implicit def Tuple2Each[R]: Each[PartialApply1Of2[Tuple2, R]#Apply] = new Each[PartialApply1Of2[Tuple2, R]#Apply] {
    def each[A](e: (R, A), f: A => Unit) = f(e._2)
  }

  implicit def Tuple3Each[R, S]: Each[PartialApply2Of3[Tuple3, R, S]#Apply] = new Each[PartialApply2Of3[Tuple3, R, S]#Apply] {
    def each[A](e: (R, S, A), f: A => Unit) = f(e._3)
  }

  implicit def Tuple4Each[R, S, T]: Each[PartialApply3Of4[Tuple4, R, S, T]#Apply] = new Each[PartialApply3Of4[Tuple4, R, S, T]#Apply] {
    def each[A](e: (R, S, T, A), f: A => Unit) = f(e._4)
  }

  implicit def Tuple5Each[R, S, T, U]: Each[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] = new Each[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] {
    def each[A](e: (R, S, T, U, A), f: A => Unit) = f(e._5)
  }

  implicit def Tuple6Each[R, S, T, U, V]: Each[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] = new Each[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] {
    def each[A](e: (R, S, T, U, V, A), f: A => Unit) = f(e._6)
  }

  implicit def Tuple7Each[R, S, T, U, V, W]: Each[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] = new Each[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] {
    def each[A](e: (R, S, T, U, V, W, A), f: A => Unit) = f(e._7)
  }

  implicit def Function0Each: Each[Function0] = new Each[Function0] {
    def each[A](e: Function0[A], f: A => Unit) = f(e.apply)
  }

  implicit def OptionEachEach: Each[Option] = new Each[Option] {
    def each[A](e: Option[A], f: A => Unit) = e foreach f
  }

  implicit def GenericArrayEach: Each[GArray] = new Each[GArray] {
    def each[A](e: GArray[A], f: A => Unit) = e foreach f
  }

  implicit def EitherLeftEach[X]: Each[PartialApply1Of2[Either.LeftProjection, X]#Flip] = new Each[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def each[A](e: Either.LeftProjection[A, X], f: A => Unit) = e foreach f
  }

  implicit def EitherRightEach[X]: Each[PartialApply1Of2[Either.RightProjection, X]#Apply] = new Each[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def each[A](e: Either.RightProjection[X, A], f: A => Unit) = e foreach f
  }

  implicit def IterableEach: Each[Iterable] = new Each[Iterable] {
    def each[A](e: Iterable[A], f: A => Unit) = e foreach f
  }

  import concurrent.Promise
  implicit def PromiseEach: Each[Promise] = new Each[Promise] {
    def each[A](e: Promise[A], f: A => Unit) = f(e.get)
  }

  implicit def JavaIterableEach: Each[java.lang.Iterable] = new Each[java.lang.Iterable] {
    def each[A](e: java.lang.Iterable[A], f: A => Unit) = {
      val i = e.iterator

      while (i.hasNext) {
        f(i.next)
      }
    }
  }
}
