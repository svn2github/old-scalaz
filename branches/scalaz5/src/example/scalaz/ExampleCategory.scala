package scalaz

object ExampleCategory {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val f = (x: Int) => (x * 7).toString
    val g = (s: String) => s.reverse.toInt

    // Right composition of the Function1 category
    println(f ⋙ g apply 33)

    // Left composition of the Function1 category
    println(g ⋘ f apply 33)
  }
}