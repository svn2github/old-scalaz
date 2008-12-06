// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.list

import scala.collection.mutable.ListBuffer

/**
 * A List that has at least one element and therefore, a total <code>head</code> and <code>tail</code>. The structure is
 * backed by a <code>scala.List</code> and so is fully evaluated.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait NonEmptyList[+A] {
  /**
   * The first element of this list (guaranteed termination).
   */
  val head: A

  /**
   * The remainder of this list (guaranteed termination).
   */
  val tail: List[A]

  /**
   * Prepend (cons) an element to this list.
   */
  def <::[B >: A](b: B) = NonEmptyList.nel(b, head :: tail)

  /**
   * Prepends a list to this list.
   */
  def <:::[B >: A](bs: List[B]) = {
    val b = new ListBuffer[B]
    b ++= bs
    b += head
    b ++= tail
    val bb = b.toList
    NonEmptyList.nel(bb.head, bb.tail)
  }

  /**
   * Maps the given function across this list.
   */
  def map[B](f: A => B) = NonEmptyList.nel(f(head), tail.map(f))

  /**
   * Binds the given function across this list.
   */
  def flatMap[B](f: A => NonEmptyList[B])  = {
    val b = new ListBuffer[B]
    val p = f(head)
    b += p.head
    b ++= p.tail
    tail.foreach(a => {
      val p = f(a)
      b += p.head
      b ++= p.tail
    })
    val bb = b.toList
    NonEmptyList.nel(bb.head, bb.tail)
  }

  /**
   * Converts this non-empty list to a scala list.
   */
  val toList = head :: tail

  /**
   * Converts this non-empty list to a scala stream.
   */
  val toStream = Stream.cons(head, tail.projection)
}

/**
 * Functions over non-empty lists.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object NonEmptyList {
  /**
   * Constructs a non-empty list with the given head and tail.
   */
  def nel[A](a: A, as: List[A]): NonEmptyList[A] = new NonEmptyList[A] {
    val head = a
    val tail = as
  }

  /**
   * Constructs a non-empty list with one element.
   */
  def nel[A](a: A): NonEmptyList[A] = nel(a, Nil)

  /**
   * An extractor for non-empty lists that always matches on the head and tail.
   */
  def unapply[A](xs: NonEmptyList[A]): Option[(A, List[A])] = Some(xs.head, xs.tail)

  /**
   * Construct a non-empty list with the given head and tail.
   */

  /**
   * Construct a non-empty list with the given head and tail.
   */
  def apply[A](x: A, xs: A*): NonEmptyList[A] = nel(x, xs.toList)

  /**
   * Construct a non-empty list with the given head and tail.
   */
  def apply[A](x: A, xs: List[A]): NonEmptyList[A] = nel(x, xs)

  /**
   * Creates a list from the given non-empty list.
   */
  implicit def toList[A](xs: NonEmptyList[A]): List[A] = xs.toList
  
  /**
   * Creates a string from the given non-empty list.
   */
  implicit def NonEmptyListString[A](xs: NonEmptyList[Char]): String = scala.List.toString(xs.toList)

  /**
   * Creates a potential non-empty list. If the given list is empty, then <code>None</code>.
   */
  implicit def NonEmptyListOptionList[A](as: List[A]): Option[NonEmptyList[A]] = as match {
    case Nil => None
    case a :: as => Some(nel(a, as))
  }
}
