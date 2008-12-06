package scalaz.control

import reductios.Property._
import reductio.CheckResult.summaryEx
import reductios.Arbitrary.SFInvariant._
import MonadEmptyLaws.{leftEmptyIdentity, rightEmptyIdentity}

object CheckMonadEmpty {
  val props = List(leftEmptyIdentity[Option, Int, String],
                   rightEmptyIdentity[Option, Int],
                   leftEmptyIdentity[List, Int, String],
                   rightEmptyIdentity[List, Int],
                   leftEmptyIdentity[Stream, Int, String],
                   rightEmptyIdentity[Stream, Int],
                   leftEmptyIdentity[Array, Int, String],
                   rightEmptyIdentity[Array, Int])
  
  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run
}
