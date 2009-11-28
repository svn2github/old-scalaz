package scalaz


object HigherKindInferenceBug {
  import Scalaz._
  
  List(1, 2, 3).v // MA.ma inferred

  implicit val a = List[Int]()

  // again relying on implicit conversion to MA. This time we call try to call foo
  // List(1, 2, 3).foo // error: scalaz is not an enclosing class
  // List(1, 2, 3) foo // error: foo is not a member of Int

  // These compile correctly.
  ma(List(1, 2, 3)).foo
  ma(List(1, 2, 3)) foo

}
