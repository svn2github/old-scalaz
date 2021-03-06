package scalaz
package memo

import scala.collection.immutable.ListMap
import scala.collection.immutable.TreeMap
import collection.immutable.Map.EmptyMap
import collection.mutable.{GenericArray, HashMap}

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

  def genericArrayMemo[K, V](size: Int) = GenericArrayMutableAssociation.comemo(new GenericArray[V](size))

  def mutableHashMapMemo[K, V] = MapMutableAssociation.comemo(new HashMap[K, V])

  def immutableEmptyMapMemo[K, V] = ImmutableMapAssociation.comemo(new EmptyMap[K, V])

  def immutableHashMapMemo[K, V] = ImmutableMapAssociation.comemo(new scala.collection.immutable.HashMap[K, V])

  def immutableListMapMemo[K, V] = ImmutableMapAssociation.comemo(new ListMap[K, V])

  def immutableTreeMapMemo[K <% Ordered[K], V]: Memo[K, V] = {
    val ordering = new scala.Ordering[K] {
      def compare(x: K, y: K) = x.compare(y);
    }
    ImmutableMapAssociation.comemo(new TreeMap[K, V]()(ordering))
  }

  // todo 2.8 UnbalancedTreeMap not yet available
  //  def immutableUnbalancedTreeMapMemo[K <% Ordered[K], V] = ImmutableMapAssociation.comemo(new UnbalancedTreeMap[K, V])
}
