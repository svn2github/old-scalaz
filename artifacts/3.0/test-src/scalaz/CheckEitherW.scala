package scalaz

import reductios.Property._  
import reductios.Arbitrary.{arbSInt, arbString}
import reductios.Coarbitrary.coarbSInt
import reductio.CheckResult.summaryEx
import fjs.F._
import fjs.F2._
import EitherW._
import EitherW.LeftProjectionW._
import EitherW.RightProjectionW._

object CheckEitherW {
  val prop_swap = prop((e: Either[Int, Int]) => ~e == (e match {
    case Left(a) => Right(a)
    case Right(b) => Left(b)
  }))

  val prop_if = prop((e: Either[Int, Int], x: Int, y: Int) => e ? (x, y) == (if(e.isLeft) x else y))

  val prop_valueLeft = prop((e: Either[String, Int], f: Int => String) => e.left.value(f) == e.fold(x => x, f(_)))
  val prop_valueRight = prop((e: Either[Int, String], f: Int => String) => e.right.value(f) == e.fold(f(_), x => x))

  val prop_applyLeft = prop((e: Either[Int, Int], f: Either[Int => String, Int]) => e.left(f) == {for(t <- f.left; u <- e.left) yield t(u)})
  val prop_applyRight = prop((e: Either[Int, Int], f: Either[Int, Int => String]) => e.right(f) == {for(t <- f.right; u <- e.right) yield t(u)})

  val prop_fmapLeft = prop((e: Either[Int, Int], f: Int => String) => (e.left > f) == e.left.map(f))
  val prop_fmapRight = prop((e: Either[Int, Int], f: Int => String) => (e.right > f) == e.right.map(f))

  val prop_bindLeft = prop((e: Either[Int, Int], f: Int => Either[Int, String]) => (e.left >>= f) == e.left.flatMap(f))
  val prop_bindRight = prop((e: Either[Int, Int], f: Int => Either[Int, String]) => (e.right >>= f) == e.right.flatMap(f))

  val prop_anonBindLeft = prop((e: Either[Int, Int], f: Either[Int, String]) => (e.left >> f) == e.left.flatMap(t => f))
  val prop_anonBindRight = prop((e: Either[Int, Int], f: Either[Int, String]) => (e.right >> f) == e.right.flatMap(t => f))

  val props = List(prop_swap, prop_if, prop_valueLeft, prop_applyLeft, prop_applyRight, prop_fmapLeft, prop_fmapRight, prop_bindLeft, prop_bindRight, prop_anonBindLeft, prop_anonBindRight)

  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run
}
