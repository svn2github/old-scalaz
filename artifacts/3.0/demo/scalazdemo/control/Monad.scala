package scalazdemo.control

/*
join[Option, Int](Some(Some(7)))
Some(7)

join[Option, Int](Some(None))
None

join[Option, Int](None)
None

join[List, Int](List(List(1, 2, 3), List(4, 5, 6)))
List(1, 2, 3, 4, 5, 6)

join[EitherLeft, Int](Right(Right(7)))
Right(7)

join[EitherLeft, Int](Right(Left("abc")))
Left(abc)

join[EitherLeft, String](Left("abc"))
Left(abc)

sequence[Int, Option, List, List](List(Some(7), Some(8), Some(9)))
Some(List(7, 8, 9))

sequence[Int, Option, List, List](List(Some(7), None, Some(9)))
None

sequence[Int, Option, List, Array](List(Some(7), Some(8), Some(9)))
Some(Array(7, 8, 9))

sequence[Int, List, List, List](List(List(1, 2, 3), List(4, 5, 6)))
List(List(1, 4), List(1, 5), List(1, 6), List(2, 4), List(2, 5), List(2, 6), List(3, 4), List(3, 5), List(3, 6))

sequence[Int, List, List, Option](List(List(1, 2, 3), List(4, 5, 6)))
List(Some(1), Some(1), Some(1), Some(2), Some(2), Some(2), Some(3), Some(3), Some(3))

sequence[Int, EitherLeft, List, List](List(Right(7), Right(8), Right(9)))
Right(List(7, 8, 9))

sequence[Int, EitherLeft, List, List](List(Right(7), Left("abc"), Right(9)))
Left(abc)
*/
object Monad {
  import scalaz.PartialType
  import scalaz.control.Monad._

  type EitherLeft[X] = PartialType[Either, String]#Apply[X]

  val demoes = List(
    // join
    ("join[Option, Int](Some(Some(7)))", join[Option, Int](Some(Some(7)))),
    ("join[Option, Int](Some(None))", join[Option, Int](Some(None))),
    ("join[Option, Int](None)", join[Option, Int](None)),
    ("join[List, Int](List(List(1, 2, 3), List(4, 5, 6)))", join[List, Int](List(List(1, 2, 3), List(4, 5, 6)))),
    ("join[EitherLeft, Int](Right(Right(7)))", join[EitherLeft, Int](Right(Right(7)))),
    ("join[EitherLeft, Int](Right(Left(\"abc\")))", join[EitherLeft, Int](Right(Left("abc")))),
    ("join[EitherLeft, String](Left(\"abc\"))", join[EitherLeft, String](Left("abc"))),

    // sequence
    ("sequence[Int, Option, List, List](List(Some(7), Some(8), Some(9)))", sequence[Int, Option, List, List](List(Some(7), Some(8), Some(9)))),
    ("sequence[Int, Option, List, List](List(Some(7), None, Some(9)))", sequence[Int, Option, List, List](List(Some(7), None, Some(9)))),
    ("sequence[Int, Option, List, Array](List(Some(7), Some(8), Some(9)))", sequence[Int, Option, List, Array](List(Some(7), Some(8), Some(9)))),
    ("sequence[Int, List, List, List](List(List(1, 2, 3), List(4, 5, 6)))", sequence[Int, List, List, List](List(List(1, 2, 3), List(4, 5, 6)))),
    ("sequence[Int, List, List, Option](List(List(1, 2, 3), List(4, 5, 6)))", sequence[Int, List, List, Option](List(List(1, 2, 3), List(4, 5, 6)))),
    ("sequence[Int, EitherLeft, List, List](List(Right(7), Right(8), Right(9)))", sequence[Int, EitherLeft, List, List](List(Right(7), Right(8), Right(9)))),
    ("sequence[Int, EitherLeft, List, List](List(Right(7), Left(\"abc\"), Right(9)))", sequence[Int, EitherLeft, List, List](List(Right(7), Left("abc"), Right(9))))
  )
  
  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
