// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.control

/**
 * A paramorphism of an iterable that can be deconstructed into next element and remaining elements.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Paramorphism[P[_]] {
  /**
   * Fold the given function across the given environment.
   *
   * @param fa The environment to fold across.
   * @param b The beginning value of the fold.
   * @param f The function to fold.
   */
  def para[A, B](fa: P[A], b: B, f: (=> A, => P[A], B) => B): B
}

import list.NonEmptyList
import list.NonEmptyList.nel

/**
 * Functions over paramorphisms.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Paramorphism {
  /**
   * A paramorphism for <code>scala.Option</code>.
   */  
  implicit val OptionParamorphism = new Paramorphism[Option] {
    override def para[A, B](as: Option[A], b: B, f: ((=> A, => Option[A], B) => B)): B = as match {
      case None => b
      case Some(a) => f(a, None, b)
    }
  }

  /**
   * A paramorphism for <code>scala.List</code>.
   */
  implicit val ListParamorphism = new Paramorphism[List] {
    override def para[A, B](as: List[A], b: B, f: ((=> A, => List[A], B) => B)): B = as match {
      case Nil => b
      case a :: as => f(a, as, para(as, b, f))
    }
  }

  /**
   * A paramorphism for <code>scala.Stream</code>.
   */
  implicit val StreamParamorphism = new Paramorphism[Stream] {
    override def para[A, B](as: Stream[A], b: B, f: ((=> A, => Stream[A], B) => B)): B =
      if(as.isEmpty)
        b
      else
        f(as.head, as.tail, para(as.tail, b, f))
  }

  /**
   * A paramorphism for <code>NonEmptyList</code>.
   */
  implicit val NonEmptyListParamorphism = new Paramorphism[NonEmptyList] {
    override def para[A, B](as: NonEmptyList[A], b: B, f: ((=> A, => NonEmptyList[A], B) => B)): B = as match {
      case NonEmptyList(a, Nil) => f(a, as, b)
      case NonEmptyList(aa, a :: as) => {
        val z = nel(a, as)
        f(aa, z, para(z, b, f))
      }
    }
  }
}
