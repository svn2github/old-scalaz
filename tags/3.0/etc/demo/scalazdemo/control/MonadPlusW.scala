package scalazdemo.control

/*
List(1, 2, 3) <+> List(4, 5, 6)
List(1, 2, 3, 4, 5, 6)

Some(7) <+> Some(8)
Some(7)

Some(7) <+> None
Some(7)

none[Int] <+> Some(8)
Some(8)

none[Int] <+> none
None

List(1, 2, 3) <+ 4
List(1, 2, 3, 4)

Some(7) <+ 8
Some(7)

none[Int] <+ 8
Some(8)

4 <+: List(1, 2, 3)
List(4, 1, 2, 3)

8 <+: Some(7)
Some(8)

8 <+: none[Int]
Some(8)
*/
object MonadPlusW {
  import scalaz.control.MonadPlusW._
  import scalaz.OptionW.none
  
  val demoes = List(
    // <+>
    ("List(1, 2, 3) <+> List(4, 5, 6)", List(1, 2, 3) <+> List(4, 5, 6)),
    ("Some(7) <+> Some(8)", Some(7) <+> Some(8)),
    ("Some(7) <+> None", Some(7) <+> None),
    ("none[Int] <+> Some(8)", none[Int] <+> Some(8)),
    ("none[Int] <+> none", none[Int] <+> none),

    // <+
    ("List(1, 2, 3) <+ 4", List(1, 2, 3) <+ 4),
    ("Some(7) <+ 8", Some(7) <+ 8),
    ("none[Int] <+ 8", none[Int] <+ 8),

    // <+:
    ("4 <+: List(1, 2, 3)", 4 <+: List(1, 2, 3)),
    ("8 <+: Some(7)", 8 <+: Some(7)),
    ("8 <+: none[Int]", 8 <+: none[Int])
  )
  
  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
