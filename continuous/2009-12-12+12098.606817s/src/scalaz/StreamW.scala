package scalaz

sealed trait StreamW[A] {
  val value: Stream[A]

  import Scalaz._

  def ʐ: ZipStream[A] = zip(value)

  def merge(s: Stream[A]): Stream[A] =
    if (value.isEmpty) Stream.Empty
    else value.head #:: s.merge(value.tail)

  def toZipper = value match {
    case Stream.Empty => None
    case h #:: t => Some(zipper(Stream.Empty, h, t))
  }

  def zipperEnd = value match {
    case Stream.Empty => None
    case _ => {
      val x = value.reverse
      Some(zipper(x.tail, x.head, Stream.Empty))
    }
  }

  def heads: Stream[Stream[A]] = value match {
    case h #:: t => Stream(h) #:: t.heads.map(h #:: _)
    case _ => Stream.Empty
  }

  def tails: Stream[Stream[A]] = value match {
    case h #:: t => value #:: t.tails
    case _ => Stream.Empty
  }

  def zapp[B, C](fs: ZipStream[A => B => C]) = (value ʐ) ⊛ fs

  def unfoldForest[B](f: A => (B, () => Stream[A])): Stream[Tree[B]] = value.map(_.unfoldTree(f))

  def unfoldForestM[B, M[_]: Monad](f: A => M[(B, Stream[A])]): M[Stream[Tree[B]]] =
    value ↦ ((_: A).unfoldTreeM(f))
}

trait Streams {
  implicit def StreamTo[A](as: Stream[A]): StreamW[A] = new StreamW[A] {
    val value = as
  }

  implicit def StreamFrom[A](as: StreamW[A]): Stream[A] = as.value
}
