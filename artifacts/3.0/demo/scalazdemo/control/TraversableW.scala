package scalazdemo.control

/*
some("789").traverse[List, Int](f)
List(Some(7), Some(8), Some(9))

none[String].traverse[List, Int](f)
List(None)

some("789").traverse[Option, Int](g)
Some(Some(789))

some("xxx").traverse[Option, Int](g)
None

none[String].traverse[Option, Int](g)
Some(None)

some("789").traverse[EitherLeft, Int](h)
Right(Some(789))

some("xxx").traverse[EitherLeft, Int](h)
Left(java.lang.NumberFormatException: For input string: "xxx")

none[String].traverse[EitherLeft, Int](h)
Right(None)

List[String]().traverse[List, Int](f)
List(List())

List("123", "456", "789").traverse[List, Int](f)
List(List(1, 4, 7), List(1, 4, 8), List(1, 4, 9), List(1, 5, 7), List(1, 5, 8), List(1, 5, 9), List(1, 6, 7), List(1, 6, 8), List(1, 6, 9), List(2, 4, 7), List(2, 4, 8), List(2, 4, 9), List(2, 5, 7), List(2, 5, 8), List(2, 5, 9), List(2, 6, 7), List(2, 6, 8), List(2, 6, 9), List(3, 4, 7), List(3, 4, 8), List(3, 4, 9), List(3, 5, 7), List(3, 5, 8), List(3, 5, 9), List(3, 6, 7), List(3, 6, 8), List(3, 6, 9))

List[String]().traverse[Option, Int](g)
Some(List())

List("123", "xxx", "789").traverse[Option, Int](g)
None

List("123", "456", "789").traverse[Option, Int](g)
Some(List(123, 456, 789))

List[String]().traverse[EitherLeft, Int](h)
Right(List())

List("123", "456", "789").traverse[EitherLeft, Int](h)
Right(List(123, 456, 789))

List("123", "xxx", "789").traverse[EitherLeft, Int](h)
Left(java.lang.NumberFormatException: For input string: "xxx")

left[String, Int]("789").traverse[List, Int](n => (1 to n).toList)
List(Left(789))

right[Int, String]("789").traverse[List, Int](f)
List(Right(7), Right(8), Right(9))

left[String, String]("789").traverse[Option, Int](g)
Some(Left(789))

right[Int, String]("xxx").traverse[Option, Int](g)
None

right[Int, String]("789").traverse[Option, Int](g)
Some(Right(789))

left[String, String]("789").traverse[EitherLeft, Int](h)
Right(Left(789))

right[Int, String]("xxx").traverse[EitherLeft, Int](h)
Left(java.lang.NumberFormatException: For input string: "xxx")

right[Int, String]("789").traverse[EitherLeft, Int](h)
Right(Right(789))

fail[String, Int]("789").traverse[List, Int](i)
List(Fail(789))

success[Int, String]("789").traverse[List, Int](f)
List(Success(7), Success(8), Success(9))

fail[String, String]("789").traverse[Option, Int](g)
Some(Fail(789))

success[Int, String]("xxx").traverse[Option, Int](g)
None

success[Int, String]("789").traverse[Option, Int](g)
Some(Success(789))

fail[String, String]("789").traverse[EitherLeft, Int](h)
Right(Fail(789))

success[Int, String]("xxx").traverse[EitherLeft, Int](h)
Left(java.lang.NumberFormatException: For input string: "xxx")

success[Int, String]("789").traverse[EitherLeft, Int](h)
Right(Success(789))

List(List(1, 2, 3), List(4, 5, 6)) ->>
List(1, 2, 3, 4, 5, 6)

List("abc", "def") ->>
abcdef

Some("abc") ->>
abc

none[Int] ->>
0

List(2, 6, 7, 8, 9) ->>
32

List(false, false, true, false, true) ->>
true
*/
object TraversableW {
  import scalaz.control.TraversableW._
  import scalaz.PartialType
  import scalaz.OptionW.{some, none}
  import scalaz.EitherW.{throws, left, right}  
  import scalaz.validation.Validation.{success, fail}
  import scalaz.control.Monoid.Plus._
  import scalaz.control.Monoid.Disjunction._

  val f = (_: String).map(_ - 48).toList
  val g = (s: String) => h(s).right.toOption
  val h = (s: String) => throws(Integer.parseInt(s))
  val i = (n: Int) => (1 to n).toList

  type EitherLeft[X] = PartialType[Either, Throwable]#Apply[X]

  val demoes = List(
    // OptionTraversable
    ("some(\"789\").traverse[List, Int](f)", some("789").traverse[List, Int](f)),
    ("none[String].traverse[List, Int](f)", none[String].traverse[List, Int](f)),
    ("some(\"789\").traverse[Option, Int](g)", some("789").traverse[Option, Int](g)),
    ("some(\"xxx\").traverse[Option, Int](g)", some("xxx").traverse[Option, Int](g)),
    ("none[String].traverse[Option, Int](g)", none[String].traverse[Option, Int](g)),
    ("some(\"789\").traverse[EitherLeft, Int](h)", some("789").traverse[EitherLeft, Int](h)),
    ("some(\"xxx\").traverse[EitherLeft, Int](h)", some("xxx").traverse[EitherLeft, Int](h)),
    ("none[String].traverse[EitherLeft, Int](h)", none[String].traverse[EitherLeft, Int](h)),

    // ListTraversable
    ("List[String]().traverse[List, Int](f)", List[String]().traverse[List, Int](f)),
    ("List(\"123\", \"456\", \"789\").traverse[List, Int](f)", List("123", "456", "789").traverse[List, Int](f)),
    ("List[String]().traverse[Option, Int](g)", List[String]().traverse[Option, Int](g)),
    ("List(\"123\", \"xxx\", \"789\").traverse[Option, Int](g)", List("123", "xxx", "789").traverse[Option, Int](g)),
    ("List(\"123\", \"456\", \"789\").traverse[Option, Int](g)", List("123", "456", "789").traverse[Option, Int](g)),
    ("List[String]().traverse[EitherLeft, Int](h)", List[String]().traverse[EitherLeft, Int](h)),
    ("List(\"123\", \"456\", \"789\").traverse[EitherLeft, Int](h)", List("123", "456", "789").traverse[EitherLeft, Int](h)),
    ("List(\"123\", \"xxx\", \"789\").traverse[EitherLeft, Int](h)", List("123", "xxx", "789").traverse[EitherLeft, Int](h)),

    // EitherTraversable
    ("left[String, Int](\"789\").traverse[List, Int](n => (1 to n).toList)", left[String, Int]("789").traverse[List, Int](i)),
    ("right[Int, String](\"789\").traverse[List, Int](f)", right[Int, String]("789").traverse[List, Int](f)),
    ("left[String, String](\"789\").traverse[Option, Int](g)", left[String, String]("789").traverse[Option, Int](g)),
    ("right[Int, String](\"xxx\").traverse[Option, Int](g)", right[Int, String]("xxx").traverse[Option, Int](g)),
    ("right[Int, String](\"789\").traverse[Option, Int](g)", right[Int, String]("789").traverse[Option, Int](g)),
    ("left[String, String](\"789\").traverse[EitherLeft, Int](h)", left[String, String]("789").traverse[EitherLeft, Int](h)),
    ("right[Int, String](\"xxx\").traverse[EitherLeft, Int](h)", right[Int, String]("xxx").traverse[EitherLeft, Int](h)),
    ("right[Int, String](\"789\").traverse[EitherLeft, Int](h)", right[Int, String]("789").traverse[EitherLeft, Int](h)),

    // ValidationTraversable
    ("fail[String, Int](\"789\").traverse[List, Int](i)", fail[String, Int]("789").traverse[List, Int](i)),
    ("success[Int, String](\"789\").traverse[List, Int](f)", success[Int, String]("789").traverse[List, Int](f)),
    ("fail[String, String](\"789\").traverse[Option, Int](g)", fail[String, String]("789").traverse[Option, Int](g)),
    ("success[Int, String](\"xxx\").traverse[Option, Int](g)", success[Int, String]("xxx").traverse[Option, Int](g)),
    ("success[Int, String](\"789\").traverse[Option, Int](g)", success[Int, String]("789").traverse[Option, Int](g)),
    ("fail[String, String](\"789\").traverse[EitherLeft, Int](h)", fail[String, String]("789").traverse[EitherLeft, Int](h)),
    ("success[Int, String](\"xxx\").traverse[EitherLeft, Int](h)", success[Int, String]("xxx").traverse[EitherLeft, Int](h)),
    ("success[Int, String](\"789\").traverse[EitherLeft, Int](h)", success[Int, String]("789").traverse[EitherLeft, Int](h)),

    // ->>
    ("List(List(1, 2, 3), List(4, 5, 6)) ->>", List(List(1, 2, 3), List(4, 5, 6)) ->>),
    ("List(\"abc\", \"def\") ->>", List("abc", "def") ->>),
    ("Some(\"abc\") ->>", Some("abc") ->>),
    ("none[Int] ->>", none[Int] ->>),
    // scalaz.control.Monoid.Plus
    ("List(2, 6, 7, 8, 9) ->>", List(2, 6, 7, 8, 9) ->>),
    // scalaz.control.Monoid.Disjunction
    ("List(false, false, true, false, true) ->>", List(false, false, true, false, true) ->>)
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
