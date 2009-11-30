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
    (x :-> fr) println

    // Map (+1) across the left of the Either binary functor
    (fl <-: x) println

    // Map reverse across the right and (+1) across the left of the Either binary functor
    (fl <-: x :-> fr) println

    // Map reverse across the right of the Tuple2 (pair) binary functor
    (y :-> fr) println

    // Map (+1) across the left of the Tuple2 (pair) binary functor
    (fl <-: y) println

    // Map reverse across the right and (+1) across the left of the Tuple2 (pair) binary functor
    (fl <-: y :-> fr) println

    // Map reverse across the right of the Either binary functor
    (z :-> fr) println

    // Map (+1) across the left of the Either binary functor
    (fl <-: z) println

    // Map reverse across the right and (+1) across the left of the Either binary functor
    (fl <-: z :-> fr) println
  }
}