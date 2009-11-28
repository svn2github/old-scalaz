package scalaz

object ExampleBifunctor {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val x: Either[Int, String] = Left(7)
    val y = (8, "abc")
    val z: Either[Int, String] = Right("def")

    val fr = ((_: String).reverse)
    val fl = ((_: Int) + 1)

    println(x :-> fr)
    println(fl <-: x)
    println(fl <-: x :-> fr)
    println(y :-> fr)
    println(fl <-: y)
    println(fl <-: y :-> fr)
    println(z :-> fr)
    println(fl <-: z)
    println(fl <-: z :-> fr)
  }
}