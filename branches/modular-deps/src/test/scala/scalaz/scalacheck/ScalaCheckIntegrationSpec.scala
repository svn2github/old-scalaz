package scalaz.scalacheck


import org.specs.Specification

object ScalaCheckIntegrationSpec extends Specification {
  import org.scalacheck._
  import scalaz.ScalazCore._
  import scalaz.ScalazScalaCheck._

  "can traverse Gen" in {
    // todo: find a way to avoid this import
    //
    // see SLR 7.2:
    //  Second, eligible are also all implicit members of some object that
    //  belongs to the implicit scope of the implicit parameter’s type, T .
    // 
    // Perhaps the implicit conversions that define type class instances for scalacheck
    // should be mixed into ScalazScalacheck.

    import scalaz.scalacheck.Applicative._
    val listgen = List(1, 1, 2, 5, 8).traverse[Gen](Gen.choose(0, _))
    listgen(Gen.Params(1, new java.util.Random(42))) must be_==(Some(List(1, 1, 1, 4, 6)))
  }
}