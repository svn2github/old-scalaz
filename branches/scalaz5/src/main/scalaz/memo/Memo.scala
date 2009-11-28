package scalaz.memo

import collection.mutable.HashMap
import collection.immutable.{ListMap, TreeMap}

trait Memo[K, V] {
  def apply(z: K => V): K => V

  def const[T] = Comemo.comemo[T, K, V](_ => this)
}

object Memo {
  def memo[K, V](f: (K => V) => K => V) = new Memo[K, V] {
    def apply(z: K => V) = f(z)
  }
  
  def nilMemo[K, V] = memo[K, V](z => z)

  import MutableAssociation._
  import ImmutableAssociation._

  def mutableHashMapMemo[K, V] = MapMutableAssociation.comemo(new HashMap[K, V])

  def immutableHashMapMemo[K, V] = ImmutableMapAssociation.comemo(new scala.collection.immutable.HashMap[K, V])

  def immutableListMapMemo[K, V] = ImmutableMapAssociation.comemo(new ListMap[K, V])

  def immutableTreeMapMemo[K <: Ordered[K], V] = ImmutableMapAssociation.comemo(new TreeMap[K, V])
}
