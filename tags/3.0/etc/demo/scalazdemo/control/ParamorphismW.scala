package scalazdemo.control

/*
List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) |- even
(List(2, 6, 8),List(9, 6, 7, 3, 5, 8, 6, 9))

List(6, 7, 3, 5, 8, 6, 9) |- even
(List(6),List(7, 3, 5, 8, 6, 9))

List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) !- even
(List(),List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9))

List(7, 3, 5, 8, 6, 9) !- even
(List(7, 3, 5),List(8, 6, 9))
*/
object ParamorphismW {
  import scalaz.control.ParamorphismW._

  val even = ((_: Int) % 2 == 0)

  val demoes = List(
    // |-
    ("List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) |- even", List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) |- even),
    ("List(6, 7, 3, 5, 8, 6, 9) |- even", List(6, 7, 3, 5, 8, 6, 9) |- even),

    // !-
    ("List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) !- even", List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) !- even),
    ("List(7, 3, 5, 8, 6, 9) !- even", List(7, 3, 5, 8, 6, 9) !- even)
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
