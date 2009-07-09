package scalaz

import org.specs.{ScalaCheck, Specification}

object BasicUsageSpec extends Specification {
  import scalaz.Scalaz._
  "Identity over standard types" should {
    "any value can be converted to" in {
      1.value must be_==(1)
      "a".value must be_==("a")
    }

    "semigroup append" in {
      "a" |+| "b" must be_==("ab")
      1 |+| 2 must be_==(3)
      List(1) |+| List(2) must be_==(List(1, 2))
    }

    "type safe equal" in {
      1 === 1 must beTrue
      "a" === "b" must beFalse
      ("a" /= "b") must beTrue
    }

    "order" in {
      1 ?:? 2 must be_==(LT)
      1 ?:? 2 must be_==(LT)
      2 ?:? 2 must be_==(EQ)
      List(1) ?:? List(2) must be_==(LT)
    }

    "shows" in {
      (List(1), "a").shows must be_==("([1], a)")
    }
  }

  "Customise behaviour" should {
    sealed case class Complex(a: Int, b: Int)

    "semigroup append" in {
      implicit val ComplexSemigroup = new Semigroup[Complex] {
        def append(c1: Complex, c2: => Complex): Complex = (c1, c2) match {
          case (Complex(a1, b1), Complex(a2, b2)) => Complex(a1 |+| a2, b1 |+| b2)
        }
      }
      Complex(1, 2) |+| Complex(3, 4) must be_==(Complex(4, 6))
    }

    "equality based on 'a' only" in {
      implicit val OneDimensionalEqual = new Equal[Complex] {
        def equal(c1: Complex, c2: Complex) = (c1, c2) match {
          case (Complex(a1, b1), Complex(a2, b2)) => a1 === a2
        }
      }
      Complex(1, 2) === Complex(1, 4) must beTrue
      List(Complex(1, 2)) === List(Complex(1, 4)) must beTrue
    }

    "show" in {
      implicit val ComplexShow = new Show[Complex] {
        def show(a: Complex) = a match {
          case Complex(a, b) => (a.toString |+| " + " |+| b.toString |+| "i").toList
        }
      }
      Complex(1, 2).shows must be_==("1 + 2i")
      List(Complex(1, 2)).shows must be_==("[1 + 2i]")
    }
  }
}
