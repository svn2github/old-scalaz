package scalaz


object HigherKindInferenceBug {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    {
      implicit val a = List[Int]()
      ma(List(1, 2, 3)) foo
//      List(1, 2, 3).foo // error: scalaz is not an enclosing class
//      List(1, 2, 3) foo // error: foo is not a member of Int
    }
  }
}
