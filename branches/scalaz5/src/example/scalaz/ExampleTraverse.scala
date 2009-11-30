package scalaz

object ExampleTraverse {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    println(List(Some(7), Some(9)).sequence[Option, Int]) // todo why must these type parameters be passed?
    println(Stream(Some(7), None, Some(9)).sequence[Option, Int]) // todo why must these type parameters be passed?

    val f = (_: String).map(_ - 48).toList
    val g = (s: String) => (() => s.parseInt).throws.either.right.toOption

    println(List("abc", "def") ↦ g)
    println(List("7", "8") ↦ g)
    println(some("abc") ↦ g)
    println(some("9") ↦ g)
  }
}
