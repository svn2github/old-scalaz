package scalazdemo.control

/*
List(2, 4, 6, 1, 2, 3, 4, 5, 6) dropSelect(even)
List(1, 2, 3, 4, 5, 6)

Some(7) dropSelect even
Some(7)

Some(8) dropSelect even
None

none[Int] dropSelect even
None
*/
object MonadEmptyPlusW {
  import scalaz.control.MonadEmptyPlusW._
  import scalaz.OptionW.none

  val even = ((_: Int) % 2 == 0)
  
  val demoes = List(
    // dropSelect
    ("List(2, 4, 6, 1, 2, 3, 4, 5, 6) dropSelect(even)", List(2, 4, 6, 1, 2, 3, 4, 5, 6) dropSelect even),
    ("Some(7) dropSelect even", Some(7) dropSelect even),
    ("Some(8) dropSelect even", Some(8) dropSelect even),
    ("none[Int] dropSelect even", none[Int] dropSelect even)
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
