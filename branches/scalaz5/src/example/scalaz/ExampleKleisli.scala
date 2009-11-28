package scalaz

object ExampleKleisli {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val f = ☆((n: Int) => if(n % 2 == 0) None else Some((n + 1).toString))
    val g = ☆((s: String) => if(List(5, 7) ∃ (_ == s.length)) None else Some("[" + s + "]"))
  
    List(7, 78, 98, 99, 100, 102, 998, 999, 10000) ➡ (n => println(f >=> g apply n))
  }
}