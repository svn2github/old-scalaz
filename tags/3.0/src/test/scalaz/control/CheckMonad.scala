package scalaz.control

import reductios.Property._
import reductio.CheckResult.summaryEx
import reductios.Arbitrary.SFInvariant._
import MonadLaws.{functorMonad, leftIdentity, rightIdentity, associativity}
import list.NonEmptyList
import list.ArbitraryNonEmptyList._

object CheckMonad {
  val props = List(functorMonad[Option, Int, String],
                   leftIdentity[Option, Int, String],
                   rightIdentity[Option, Int],
                   associativity[Option, Int, String, Long],
                   functorMonad[List, Int, String],
                   leftIdentity[List, Int, String],
                   rightIdentity[List, Int],
                   associativity[List, Int, String, Long],
                   functorMonad[Stream, Int, String],
                   leftIdentity[Stream, Int, String],
                   rightIdentity[Stream, Int],
                   associativity[Stream, Int, String, Long],
                   functorMonad[Array, Int, String],
                   leftIdentity[Array, Int, String],
                   rightIdentity[Array, Int],
                   associativity[Array, Int, String, Long],
                   functorMonad[NonEmptyList, Int, String],
                   leftIdentity[NonEmptyList, Int, String],
                   rightIdentity[NonEmptyList, Int],
                   associativity[NonEmptyList, Int, String, Long],
                   functorMonad[PartialType[Either, Long]#Apply, Int, String],
                   leftIdentity[PartialType[Either, Long]#Apply, Int, String],
                   rightIdentity[PartialType[Either, Long]#Apply, Int])

  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run
}
