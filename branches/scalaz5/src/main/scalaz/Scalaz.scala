package scalaz

object Scalaz extends ScalazLow
    with    Alphas
    with    Applys
    with    ArrayBytes
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
    with    Digits
    with    DLists
    with    Duals
    with    Endos
    with    Enumerations
    with    Equals
    with    FirstOptions
    with    Function0s
    with    Function1s
    with    Function2s
    with    Kleislis
    with    Identitys
    with    InputStreams
    with    IntMultiplications
    with    Ints
    with    Iterables
    with    LastOptions
    with    Lists
    with    LongMultiplications
    with    Longs
    with    MetricSpaces
    with    Memos
    with    NonEmptyLists
    with    Options
    with    Orders
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

  type GArray[A] = collection.mutable.GenericArray[A]

  def ×[A, B] = (a: A) => (b: B) => (a, b)
}
