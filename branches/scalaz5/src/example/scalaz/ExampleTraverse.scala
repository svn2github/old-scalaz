package scalaz

object ExampleTraverse {
  def main(args: Array[String]) = run

  import Scalaz._
  import collection.mutable.GenericArray

  def run {
    // Sequence the List with the Option applicative functor
    List(some(7), some(9)).sequence println

    // Sequence the Stream with the Option applicative functor
    Stream(Some(7), None, Some(9)).sequence println 

    val f = (_: String).map(_ - 48).toList
    val g = (s: String) => (() => s.parseInt).throws.either.right.toOption

    // Traverse the List with the Option applicative functor (domain of g)
    (List("abc", "def") ↦ g) println

    // Traverse the List with the Option applicative functor (domain of g)
    (List("7", "8") ↦ g) println

    // Traverse the Option with the Option applicative functor (domain of g)
    (some("abc") ↦ g) println

    // Traverse the Option with the Option applicative functor (domain of g)
    (some("9") ↦ g) println

    // Traverse a List of characters to get a possible List of digits (scalaz.Digit) using the Option applicative functor
    List('1', '2', '3').traverseDigits println

    // Traverse an Option of characters to get a possible Option of digits (scalaz.Digit) using the Option applicative functor
    some('1').traverseDigits println

    // Traverse a GenericArray of characters to get a possible GenericArray of digits (scalaz.Digit) using the Option applicative functor
    GenericArray('1', 'x', '3').traverseDigits println

    // Traverse a List using the String monoid
    List(100, 200, 300) ↣ (_.toString) println

    // Traverse a GenericArray using the Int addition monoid
    GenericArray(100, 200, 300) ↣ (x => x) println
  
    // Traverse a Stream using the Int multiplication monoid
    Stream(100, 200, 300) ↣ (x => x ∏) println

    // Traverse an Option using the Int multiplication monoid
    some(100) ↣ (x => x ∏) println

    // Traverse an Option using the Int multiplication monoid
    none[Long] ↣ (x => x ∏) println
  }
}
