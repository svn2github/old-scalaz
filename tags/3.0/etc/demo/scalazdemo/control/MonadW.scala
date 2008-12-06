package scalazdemo.control

/*
List("a", "b", "c").replicate[List](3)
List(List(a, a, a), List(a, a, b), List(a, a, c), List(a, b, a), List(a, b, b), List(a, b, c), List(a, c, a), List(a, c, b), List(a, c, c), List(b, a, a), List(b, a, b), List(b, a, c), List(b, b, a), List(b, b, b), List(b, b, c), List(b, c, a), List(b, c, b), List(b, c, c), List(c, a, a), List(c, a, b), List(c, a, c), List(c, b, a), List(c, b, b), List(c, b, c), List(c, c, a), List(c, c, b), List(c, c, c))

List("a", "b", "c").replicate[Option](3)
List(Some(a), Some(b), Some(c))

Some("a").replicate[List](3)
Some(List(a, a, a))

none[String].replicate[List](3)
None

none[String].replicate[List](3)
None

Left("a").replicate[List](3)
Left(a)

Right("b").replicate[List](3)
Right(List(b, b, b))

List(1, 2, 3).mapM[Int, List, List](i => (1 to i).toList)
List(List(1, 1, 1), List(1, 1, 2), List(1, 1, 3), List(1, 2, 1), List(1, 2, 2), List(1, 2, 3))

List(1, 2, 3).mapM[Int, Array, List](i => (1 to i).toList)
List(Array(1, 1, 1), Array(1, 1, 2), Array(1, 1, 3), Array(1, 2, 1), Array(1, 2, 2), Array(1, 2, 3))

List(1, 2, 3).mapM[Int, List, Option](Some(_))
Some(List(1, 2, 3))

List(1, 2, 3).mapM[Int, List, Option](n => if(n == 4) None else Some(n))
Some(List(1, 2, 3))

List(1, 2, 3).mapM[Int, List, Option](n => if(n == 3) None else Some(n))
None
*/
object MonadW {
  import scalaz.control.MonadW._
  import scalaz.OptionW.none

  val demoes = List(
    // replicate
    ("List(\"a\", \"b\", \"c\").replicate[List](3)", List("a", "b", "c").replicate[List](3)),
    ("List(\"a\", \"b\", \"c\").replicate[Option](3)", List("a", "b", "c").replicate[Option](3)),
    ("Some(\"a\").replicate[List](3)", Some("a").replicate[List](3)),
    ("none[String].replicate[List](3)", none[String].replicate[List](3)),
    ("none[String].replicate[List](3)", none[String].replicate[List](3)),
    ("Left(\"a\").replicate[List](3)", Left("a").replicate[List](3)),
    ("Right(\"b\").replicate[List](3)", Right("b").replicate[List](3)),

    // mapM
    ("List(1, 2, 3).mapM[Int, List, List](i => (1 to i).toList)", List(1, 2, 3).mapM[Int, List, List](i => (1 to i).toList)),
    ("List(1, 2, 3).mapM[Int, Array, List](i => (1 to i).toList)", List(1, 2, 3).mapM[Int, Array, List](i => (1 to i).toList)),
    ("List(1, 2, 3).mapM[Int, List, Option](Some(_))", List(1, 2, 3).mapM[Int, List, Option](Some(_))),
    ("List(1, 2, 3).mapM[Int, List, Option](n => if(n == 4) None else Some(n))", List(1, 2, 3).mapM[Int, List, Option](n => if(n == 4) None else Some(n))),
    ("List(1, 2, 3).mapM[Int, List, Option](n => if(n == 3) None else Some(n))", List(1, 2, 3).mapM[Int, List, Option](n => if(n == 3) None else Some(n)))
  )
  
  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
