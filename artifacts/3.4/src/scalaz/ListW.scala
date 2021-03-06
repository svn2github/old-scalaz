// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * Wraps <code>scala.List</code> and provides additional methods.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait ListW[A] {
  /**
   * The value of this list.
   */
  val list: List[A]

  /**
   * Converts this list to a string with each element corresponding to a character.
   */
  def string(f: A => Char) = list map f mkString

  /**
   * Converts this list to a string with each element corresponding to a list of characters.
   */
  def stringj(f: A => List[Char]) = list flatMap f mkString

  import scalaz.list.NonEmptyList

  /**
   * Returns the first argument if this is an empty list or runs the given function on the head and tail.
   */
  def |*|[X](e: => X) = new {
    def |**|(f: NonEmptyList[A] => X) = list match {
      case Nil => e
      case h :: t => f(NonEmptyList.nel(h, t))
    }
  }

  /**
   * Sequences each element of this list through a monad - given by a function to map on each element.
   */
  def sequence[M[_]] = new {
    def apply[B](f: A => M[B])(implicit m: control.Monad[M]) =
      (list map f).foldRight[M[List[B]]](m.pure(Nil))((a, b) => m.bind((j: B) => m.fmap(j :: (_: List[B]), b), a))
  }

  import ListW._

  /**
   * Intersperses the given element between each element of the list.
   */
  def intersperse(a: A): List[A] = list match {
    case Nil => Nil
    case x :: Nil => x :: Nil
    case h :: t => h :: a :: ListListW(t).intersperse(a)
  }

  /**
   * Intercalates the given elements between each element of the list.
   */
  def intercalate(as: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: Nil => x :: Nil
    case h :: t => h :: as ::: ListListW(t).intercalate(as)
  }

  /**
   * Returns the first argument if the list is empty, otherwise, executes the given function.
   */
  def empty[X](x: => X, f: NonEmptyList[A] => X) = list match {
    case Nil => x
    case h :: t => f(NonEmptyList.nel(h, t))
  }

  import scalaz.EqualW._

  /**
   * Removes duplicate elements in O(n^2) time.
   */
  def nub(implicit e: Equal[A]): List[A] = list match {
    case Nil => Nil
    case h :: t => h :: ListListW(t.filter(y => h /= y)).nub 
  }

  /**
   * Removes duplicate elements in O(n^2) time.
   */
  def nubBy(f: (A, A) => Boolean) = nub(Equal.equal(f))

  import xml.{NodeSeq, Text}

  /**
   * Returns an empty XML text node if this is an empty list or runs the given function on the head and tail.
   */
  def <^>(f: NonEmptyList[A] => NodeSeq) = |*|(Text(""): NodeSeq) |**| f 
}

/**
 * Functions over list values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ListW {
  /**
   * Unwraps a <code>scala.List</code>.
   */
  implicit def ListWList[A](as: ListW[A]) = as.list

  /**
   * Wraps a <code>scala.List</code>.
   */
  implicit def ListListW[A](as: List[A]) = new ListW[A] {
    val list = as
  }
}
