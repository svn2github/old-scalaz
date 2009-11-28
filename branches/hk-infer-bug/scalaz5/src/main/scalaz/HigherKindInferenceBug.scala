package scalaz


object HigherKindInferenceBug {
  import Scalaz._
  
  List(1, 2, 3).v // higher kinded type param to MA.ma inferred, we can access the value `v` on the converted MA.

  implicit val a = List[Int]()

  // again relying on implicit conversion to MA. This time we call try to call foo, a method
  // that itself takes an implicit parameter involving the type param A of MA.
//  List(1, 2, 3).foo // error: scalaz is not an enclosing class
//  List(1, 2, 3) foo // error: foo is not a member of Int
//  List(1, 2, 3).foo(a) // error: type mismatch, found: Int, required List[Int]


  // These compile correctly.
  ma(List(1, 2, 3)).foo
  ma(List(1, 2, 3)) foo


  // we can call bar successfully, which does not have an implicit parameter.
  List(1, 2, 3).bar(a) // error: scalaz is not an enclosing class
  List(1, 2, 3) bar(a) // error: foo is not a member of Int
}
