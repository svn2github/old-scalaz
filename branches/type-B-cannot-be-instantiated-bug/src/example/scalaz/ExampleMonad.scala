package scalaz

object ExampleMonad {
  def main(args: Array[String]) = run

  import Scalaz._
  import collection.mutable.GenericArray
  
  def run {
    // zeroOr
    some(7).zeroOr[List].println
    none[Int].zeroOr[List].println
    some(7).zeroOr[GenericArray].println
    some(7).zeroOr[Option].println

    // guard
    true.guard[List](7).println
    false.guard[List](7).println
    true.guard[Option](7).println
    false.guard[Option](7).println

    // prevent
    true.prevent[List](8).println
    false.prevent[List](8).println
    true.prevent[Option](8).println
    false.prevent[Option](8).println

    // join μ
    (some(some(9)) μ).println
    (some(none[Int]) μ).println
    (none[Option[Int]] μ).println
    (List(List(1, 2, 3), List(4, 5, 6)) μ).println

    // bind ∗
    (List(1, 2, 3) ∗ (List(7, _))).println
    (some(7) ∗ (x => if(x % 2 == 0) some(x - 1) else none)).println
    (some(8) ∗ (x => if(x % 2 == 0) some(x - 1) else none)).println

    // Folding left on a List through the List monad
    List(1, 2, 3, 4).foldLeftM((b: Int, a: Int) => List(10, 20, a, b), 0).println

    // Folding left on a Stream through the Option monad
    Stream(1, 2, 3, 4).foldLeftM((b: Int, a: Int) => some(a + b), 0).println

    // Folding right on a Stream through the List monad
    Stream(1, 2, 3, 4).foldRightM((b: Int, a: Int) => List(10, 20, a, b), 0).println

    // Folding right on a List through the Option monad
    List(1, 2, 3, 4).foldRightM((b: Int, a: Int) => some(a + b), 0).println

    // Take-while on a List through the Option monad
    List.range(1, 50).takeWhileM(n => if(n < 100) some(n < 10) else none).println

    // Take-while on a List through the List monad
    List.range(1, 50).takeWhileM(n => if(n < 40) List(n < 10, n < 20) else List(n % 7 > 0)).println

    // Filtering on a List through the List monad (produces the powerset)
    List(1, 2, 3).filterM(_ => List(true, false)).println

  // Filtering on a List through the Option monad 
    List(1, 2, 3).filterM(n => some(n < 3)).println

    // Replicating a List through the List monad
    List(1, 2, 3).replicateM[List](2).println

    // Replicating a List through the Option monad
    List(1, 2, 3).replicateM[Option](2).println

    // Replicating an Option through the GenericArray monad
    some(7).replicateM[GenericArray](3).println

    // Replicating an Option through the Option monad
    some(7).replicateM[Option](3).println
  }
}
