package scalaz

object ExampleBifunctor {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val x: Either[Int, String] = Left(7)
    val y = (8, "abc")
    val z: Either[Int, String] = Right("def")

    val fr = (_: String).reverse
    val fl = (_: Int) + 1

    // Map reverse across the right of the Either binary functor
    println(x :-> fr)

    // Map (+1) across the left of the Either binary functor
    println(fl <-: x)

    // Map reverse across the right and (+1) across the left of the Either binary functor
    println(fl <-: x :-> fr)

    // Map reverse across the right of the Tuple2 (pair) binary functor
    println(y :-> fr)

    // Map (+1) across the left of the Tuple2 (pair) binary functor
    println(fl <-: y)

    // Map reverse across the right and (+1) across the left of the Tuple2 (pair) binary functor
    println(fl <-: y :-> fr)

    // Map reverse across the right of the Either binary functor
    println(z :-> fr)

    // Map (+1) across the left of the Either binary functor
    println(fl <-: z)

    // Map reverse across the right and (+1) across the left of the Either binary functor
    println(fl <-: z :-> fr)
  }
}