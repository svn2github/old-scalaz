package scalaz

object ExampleArrow {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val plus1 = (_: Int) + 1
    val times2 = (_: Int) * 2
    val rev = (_: String) reverse

    // Function1 arrow
    {
      // Applying first on the Function1 arrow.
      plus1.first apply (7, "abc") assert_≟ (8, "abc")

      // Applying second on the Function1 arrow.
      plus1.second apply ("def", 14) assert_≟ ("def", 15)

      // Combine plus1 and rev on the Function1 arrow to apply across both elements of a pair.
      plus1 *** rev apply (7, "abc") assert_≟ (8, "cba")

      // Perform both plus1 and times2 on a value using the Function1 arrow
      plus1 &&& times2 apply 7 assert_≟ (8, 14)

      // Perform plus1 on a pair using the Function1 arrow
      plus1.product apply (9, 99) assert_≟ (10, 100)
    }

    // Kleisli arrow
    {
      // TODO Eschew the explicit call to mab
      // mab[PartialApplyK[Kleisli, Option]#Apply, List[Int], String](☆((n: List[Int]) => if(n.isEmpty) None else Some(n.shows.reverse)))
    }
  }
}
