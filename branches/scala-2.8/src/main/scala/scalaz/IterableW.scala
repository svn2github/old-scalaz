package scalaz

import collection.JavaConversions.JIterableWrapper

sealed trait IterableW[+A] {
  val value: Iterable[A]

  def toJava[AA >: A]: java.lang.Iterable[AA] = new java.lang.Iterable[AA] {
    val i = value.iterator
    def iterator = new java.util.Iterator[AA] {
      def hasNext = i.hasNext
      def next = i.next
      def remove = error("not supported")
    }
  }
}

object IterableW {
  implicit def IterableTo[A](i: Iterable[A]) = new IterableW[A] {
    val value = i
  }

  implicit def IterableFrom[A](i: IterableW[A]) = i.value

  implicit def JavaIterableTo[A](i: java.lang.Iterable[A]): IterableW[A] = new IterableW[A] {
    val value = JIterableWrapper(i)
  }
}
