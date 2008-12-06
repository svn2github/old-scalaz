package scalaz.control

import reductios.Property._
import reductio.CheckResult.summaryEx
import reductios.Arbitrary.SFInvariant._
import MonadPlusLaws.associative
import list.NonEmptyList
import list.ArbitraryNonEmptyList._
        
object CheckMonadPlus {
  val props = List(associative[Option, Int],
                   associative[List, Int],
                   associative[Stream, Int],
                   associative[Array, Int],
                   associative[NonEmptyList, Int],
                   associative[PartialType[Either, String]#Apply, Int])

  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run
}
