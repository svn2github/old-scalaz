package scalaz

object ExampleTraverse {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Sequence the List with the Option applicative functor
    List(Some(7), Some(9)).sequence[Option, Int].println // todo why must these type parameters be passed?

    // Sequence the Stream with the Option applicative functor
    Stream(Some(7), None, Some(9)).sequence[Option, Int].println // todo why must these type parameters be passed?

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
    collection.mutable.GenericArray('1', 'x', '3').traverseDigits println

    // Traverse a List using the String monoid
    List(100, 200, 300) ↣ (_.toString) println

    List(100, 200, 300) ↣ (x => x) println
  
    List(100, 200, 300) ↣ (x => x ∏) println
  }
}
