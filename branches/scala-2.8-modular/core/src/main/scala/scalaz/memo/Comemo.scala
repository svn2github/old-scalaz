package scalaz
package memo

import collection.mutable.GenericArray

sealed trait Comemo[-T, K, V] {
  def apply(t: T): Memo[K, V]
}

object Comemo {
  def comemo[T, K, V](f: T => Memo[K, V]) = new Comemo[T, K, V] {
    def apply(t: T) = f(t)
  }

  import Scalaz._

  implicit def ComemoCofunctor[K, V] = new Cofunctor[PartialApply2Of3[Comemo, K, V]#ApplyA] {
    def comap[A, B](r: Comemo[A, K, V], f: B => A) = comemo[B, K, V](b => r(f(b)))
  }

  def genericArraySizeComemo[V] = MutableAssociation.GenericArrayMutableAssociation.comemo <| ((sz: Int) => new GenericArray[V](sz))
}