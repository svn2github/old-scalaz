package scalaz

import concurrent._
import geo._

object Scalaz extends ScalazLow
    with    Actors
    with    Alphas
    with    Applys
    with    ArrayBytes
    with    Azimuths
    with    Bearings
    with    BigIntegerMultiplications
    with    BigIntegers
    with    BigIntMultiplications
    with    BigInts
    with    BKTrees
    with    Booleans
    with    BooleanConjunctions
    with    ByteMultiplications
    with    Bytes
    with    CharMultiplications
    with    Chars
    with    CharSets
    with    Cokleislis
    with    Coords
    with    Digits
    with    DLists
    with    DoubleWs
    with    Duals
    with    Effects
    with    Elevations
    with    Ellipsoids
    with    Emptys
    with    Endos
    with    Enumerations
    with    Equals
    with    FirstOptions
    with    Function0s
    with    Function1s
    with    Function2s
    with    GeodeticCurves
    with    Kleislis
    with    Identitys
    with    InputStreams
    with    IntMultiplications
    with    Ints
    with    Iterables
    with    LastOptions
    with    Latitudes
    with    Lists
    with    Longitudes
    with    LongMultiplications
    with    Longs
    with    MAs
    with    MetricSpaces
    with    Memos
    with    NonEmptyLists
    with    Options
    with    Orders
    with    Positions
    with    Promises
    with    Radianss
    with    Semigroups
    with    ShortMultiplications
    with    Shorts
    with    Shows
    with    States
    with    Streams
    with    Strings
    with    Trees
    with    TreeLocs
    with    Validations
    with    Zeros
    with    Zippers
    with    ZipStreams {
  def ⊥ = error("undefined")

  def undefined = ⊥

  type ⊤ = Any

  type ℤ = BigInt

  lazy val π = java.lang.Math.PI

  lazy val π2 = π * 2   

  type GArray[A] = collection.mutable.GenericArray[A]

  type ![A <: Applicable, B] = A#Apply[B]

  def ×[A, B] = (a: A) => (b: B) => (a, b)

  // todo move to MAs, once https://lampsvn.epfl.ch/trac/scala/ticket/2741 is solved.
  implicit def Function1FlipMACofunctor[A, R](f: R => A): MACofunctor[PartialApply1Of2[Function1, A]#Flip, R] = maCofunctor[PartialApply1Of2[Function1, A]#Flip, R](f)

  implicit def Function1ApplyMA[A, R](f: A => R): MA[PartialApply1Of2[Function1, A]#Apply, R] = ma[PartialApply1Of2[Function1, A]#Apply, R](f)

  // Seq[A] implements Function1[Int, A]. Without this, Function1FlipMA would be used.
  implicit def SeqMA[M[_] <: Seq[_], A](l: M[A]): MA[M, A] = ma[M, A](l)
}
