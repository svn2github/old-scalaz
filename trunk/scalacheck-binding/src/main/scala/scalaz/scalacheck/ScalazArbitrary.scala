package scalaz
package scalacheck

import java.math.BigInteger
import org.scalacheck.{Pretty, Gen, Arbitrary}
import java.io._

/**
 * Instances of {@link scalacheck.Arbitrary} for many types in Scalaz.
 */
object ScalazArbitrary {
  import Scalaz._
  import Arbitrary._
  import Gen._
  import ScalaCheckBinding._

  // todo report and/or work around compilation error: "scalaz is not an enclosing class"
  // implicit def ShowPretty[A: Show](a: A): Pretty = Pretty { _ => a.show }

  private def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]

  implicit def IdentityArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Identity[A]] =
    a ∘ ((x: A) => IdentityTo(x))

  implicit def UnitArbitrary: Arbitrary[Unit] = Arbitrary(value(()))

  implicit def AlphaArbitrary: Arbitrary[Alpha] = Arbitrary(oneOf(alphas ∘ (value _): _*))

  implicit def BooleanConjunctionArbitrary: Arbitrary[BooleanConjunction] = arb[Boolean] ∘ ((_: Boolean).|∧|)

  implicit def arbBigInt: Arbitrary[BigInt] = arb[Int] <**> (arb[Int], (_: Int) * (_: Int))

  implicit def arbBigInteger: Arbitrary[BigInteger] = arb[BigInt] ∘ (_.bigInteger)

  implicit def BigIntegerMultiplicationArbitrary: Arbitrary[BigIntegerMultiplication] = arb[BigInteger] ∘ ((_: BigInteger).∏)

  implicit def BigIntMultiplicationArbitrary: Arbitrary[BigIntMultiplication] = arb[BigInt] ∘ ((_: BigInt).∏)

  implicit def ByteMultiplicationArbitrary: Arbitrary[ByteMultiplication] = arb[Byte] ∘ ((_: Byte).∏)

  implicit def CharMultiplicationArbitrary: Arbitrary[CharMultiplication] = arb[Char] ∘ ((_: Char).∏)

  implicit def ShortMultiplicationArbitrary: Arbitrary[ShortMultiplication] = arb[Short] ∘ ((_: Short).∏)

  implicit def IntMultiplicationArbitrary: Arbitrary[IntMultiplication] = arb[Int] ∘ ((_: Int).∏)

  implicit def LongMultiplicationArbitrary: Arbitrary[LongMultiplication] = arb[Long] ∘ ((_: Long).∏)

  implicit def DigitArbitrary: Arbitrary[Digit] = Arbitrary(oneOf(digits ∘ (value _): _*))

  implicit def DListArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[DList[A]] = arb[List[A]] ∘ (as => dlist(_ => as))

  implicit def NonEmptyListArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[NonEmptyList[A]] = arb[A] <**> (arb[List[A]], nel(_: A, _: List[A]))

  implicit def OrderingArbitrary: Arbitrary[Ordering] = Arbitrary(oneOf(LT, EQ, GT))

  implicit def TreeArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Tree[A]] = Arbitrary {
    def tree(n: Int): Gen[Tree[A]] = n match {
      case 0 => arbitrary[A] ∘ (leaf _)
      case n => {
        val nextSize = n.abs / 2
        arbitrary[A] <**> (resize(n, arbStream[Tree[A]](Arbitrary(tree(nextSize))).arbitrary), ((a: A, f: Stream[Tree[A]]) => node(a, f)))
      }
    }
    Gen.sized(tree _)
  }

  implicit def TreeLocArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[TreeLoc[A]] = arb[Tree[A]] ∘ ((t: Tree[A]) => t.loc)

  implicit def ValidationArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Validation[A, B]] = arb[Either[A, B]] ∘ (validation _)

  implicit def FailProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[FailProjection[A, B]] = arb[Validation[A, B]] ∘ (_.fail)

  implicit def ZipStreamArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[ZipStream[A]] = arb[Stream[A]] ∘ ((s: Stream[A]) => s.ʐ)

  implicit def Tuple1Arbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Tuple1[A]] = arb[A] ∘ ((x: A) => Tuple1(x))

  implicit def Function0Arbitrary[A](implicit a: Arbitrary[A]): Arbitrary[() => A] = arb[A] ∘ ((x: A) => () => x)

  implicit def FirstOptionArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[FirstOption[A]] = arb[Option[A]] ∘ ((x: Option[A]) => x.fst)

  implicit def LastOptionArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[LastOption[A]] = arb[Option[A]] ∘ ((x: Option[A]) => x.lst)

  implicit def EitherLeftProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.LeftProjection[A, B]] = arb[Either[A, B]] ∘ ((x: Either[A, B]) => x.left)

  implicit def EitherRightProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.RightProjection[A, B]] = arb[Either[A, B]] ∘ ((x: Either[A, B]) => x.right)

  implicit def GenericArrayArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[GArray[A]] = arb[List[A]] ∘ ((x: List[A]) => GArray(x: _*))

  import java.util.concurrent.Callable

  implicit def CallableArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Callable[A]] = arb[A] ∘ ((x: A) => x.η[Callable])

  import concurrent.Promise

  implicit def PromiseArbitrary[A](implicit a: Arbitrary[A], s: concurrent.Strategy[Unit]): Arbitrary[Promise[A]] = arb[A] ∘ ((x: A) => promise(x))

  implicit def ZipperArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Zipper[A]] = arb[Stream[A]] <***> (arb[A], arb[Stream[A]], zipper[A]((_: Stream[A]), (_: A), (_: Stream[A])))

  import geo._
  implicit def AzimuthArbitrary: Arbitrary[Azimuth] = arb[Double] ∘ (azimuth _)

  implicit def BearingArbitrary: Arbitrary[Bearing] = arb[Double] ∘ (bearing _)

  implicit def CoordArbitrary: Arbitrary[Coord] = arb[Latitude] <**> (arb[Longitude], coord _)

  implicit def ElevatedCurveArbitrary: Arbitrary[ElevatedCurve] = arb[GeodeticCurve] <**> (arb[Elevation], elevatedCurve _)

  implicit def ElevationArbitrary: Arbitrary[Elevation] = arb[Double] ∘ (elevation _)

  implicit def EllipsoidArbitrary: Arbitrary[Ellipsoid] = arb[Double] <****> (arb[Double], arb[Double], arb[Double], ellipsoid _)

  implicit def GeodeticCurveArbitrary: Arbitrary[GeodeticCurve] = arb[Double] <***> (arb[Azimuth], arb[Azimuth], curve _)

  implicit def LatitudeArbitrary: Arbitrary[Latitude] = arb[Double] ∘ (latitude _)

  implicit def LongitudeArbitrary: Arbitrary[Longitude] = arb[Double] ∘ (longitude _)

  implicit def PositionArbitrary: Arbitrary[Position] = arb[Coord] <**> (arb[Elevation], position _)

  implicit def VectorArbitrary: Arbitrary[Vector] = arb[Coord] <**> (arb[Bearing], vector _)

  trait Duplicate[A] {
    def pair: (A, A)
  }

  /**
   * An Arbitrary that calls the underlying generator twice with each random value, returning
   * a pair of identically created values. This is useful to test that equality is not based on
   * object identity.
   */
  implicit def DuplicateArbitrary[A](implicit aa: Arbitrary[A]): Arbitrary[Duplicate[A]] = Arbitrary {
    Gen {
      (params: Gen.Params) => {
        val Params(_, rng) = params
        // Because java.util.Random is side-effectful, it cannot be shared.
        // Furthermore, the only way to copy one is through serialization.
        def cloneParams = params.copy(rng = Serialization.clone(rng))
        def a: Option[A] = aa.arbitrary(cloneParams)
        (a <|*|> a) ∘ (as => new Duplicate[A] {
          def pair = as
        })
      }
    }
  }
}

private[scalacheck] object Serialization {
  def clone[A <: Serializable](obj: A): A = deserialize(serialize(obj))

  def serialize[A <: Serializable](obj: A, outputStream: OutputStream): Unit = {
    var out = new ObjectOutputStream(outputStream)
    try {
      out.writeObject(obj)
    } finally {
      out.close
    }
  }

  def deserialize[A <: Serializable](objectData: Array[Byte]): A = deserialize(new ByteArrayInputStream(objectData))

  def serialize(obj: Serializable): Array[Byte] = {
    var baos = new ByteArrayOutputStream(512)
    serialize(obj, baos)
    baos.toByteArray
  }

  def deserialize[A <: Serializable](inputStream: InputStream): A = {
    var in = new ObjectInputStream(inputStream)
    try {
      return in.readObject.asInstanceOf[A]
    }
    finally {
      in.close
    }
  }
}


