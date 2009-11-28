package scalaz.memo

sealed trait Comemo[-T, K, V] {
  def apply(t: T): Memo[K, V]

  def comap[U](f: U => T) = new Comemo[U, K, V] {
    def apply(u: U) = Comemo.this(f(u))
  }
}

object Comemo {
  def comemo[T, K, V](f: T => Memo[K, V]) = new Comemo[T, K, V] {
    def apply(t: T) = f(t)
  }

  def arraySizeComemo[V: Manifest] = MutableAssociation.ArrayMutableAssociation.comemo comap ((sz: Int) => new Array[V](sz))
}