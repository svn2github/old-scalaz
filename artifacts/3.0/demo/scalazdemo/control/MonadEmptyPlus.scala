package scalazdemo.control

/*
unfold[Char, List[Char], List](s => if(s.isEmpty) None else Some((s.head.toUpperCase, s.tail)), List('a', 'b', 'c'))
List(A, B, C)

replicate[String, List](4, "abc")
List(abc, abc, abc, abc)

replicate[String, Option](4, "abc")
Some(abc)

sum[Int, List, List](List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
List(1, 2, 3, 4, 5, 6, 7, 8, 9)

sum[Int, List, Option](List(Some(7), None, Some(8)))
Some(7)

sum[Int, List, Option](List(None, None, Some(8)))
Some(8)

sum[Int, List, Option](List(None, None, None))
None
*/
object MonadEmptyPlus {
  import scalaz.control.MonadEmptyPlus.{unfold, replicate, sum}

  val demoes = List(
    // unfold
    ("unfold[Char, List[Char], List](s => if(s.isEmpty) None else Some((s.head.toUpperCase, s.tail)), List('a', 'b', 'c'))",
            unfold[Char, List[Char], List](s => if(s.isEmpty) None else Some((s.head.toUpperCase, s.tail)), List('a', 'b', 'c'))),

    // replicate
    ("replicate[String, List](4, \"abc\")", replicate[String, List](4, "abc")),
    ("replicate[String, Option](4, \"abc\")", replicate[String, Option](4, "abc")),

    // sum
    ("sum[Int, List, List](List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))",
            sum[Int, List, List](List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))),
    ("sum[Int, List, Option](List(Some(7), None, Some(8)))",
            sum[Int, List, Option](List(Some(7), None, Some(8)))),
    ("sum[Int, List, Option](List(None, None, Some(8)))",
            sum[Int, List, Option](List(None, None, Some(8)))),
    ("sum[Int, List, Option](List(None, None, None))",
            sum[Int, List, Option](List(None, None, None)))  
  )
  
  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
