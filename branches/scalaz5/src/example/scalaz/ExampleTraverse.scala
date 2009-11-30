package scalaz

object ExampleTraverse {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Sequence the List with the Option applicative functor
    println(List(Some(7), Some(9)).sequence[Option, Int]) // todo why must these type parameters be passed?

    // Sequence the Stream with the Option applicative functor
    println(Stream(Some(7), None, Some(9)).sequence[Option, Int]) // todo why must these type parameters be passed?

    val f = (_: String).map(_ - 48).toList
    val g = (s: String) => (() => s.parseInt).throws.either.right.toOption

    // Traverse the List with the Option functor (g)
    println(List("abc", "def") ↦ g)

    // Traverse the List with the Option functor (g)
    println(List("7", "8") ↦ g)

    // Traverse the Option with the Option functor (g)
    println(some("abc") ↦ g)

    // Traverse the Option with the Option functor (g)
    println(some("9") ↦ g)
  }
}
