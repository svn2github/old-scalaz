package scalaz

object ExampleCategory {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val f = (x: Int) => (x * 7).toString
    val g = (s: String) => s.reverse.toUpperCase.toInt
    println(f ⋙ g apply 33)
    println(g ⋘ f apply 33)
  }
}