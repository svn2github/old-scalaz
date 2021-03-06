package scalaz

/**
 * A multi-way tree, also known as a rose tree.
 */
sealed trait Tree[+A] {
  val rootLabel: A

  def subForest: Stream[Tree[A]]

  import Scalaz._
  import MA._

  def foldMap[B](f: A => B)(implicit m: Monoid[B]): B =
    f(rootLabel) |+| subForest.foldMap((_: Tree[A]).foldMap(f))

  def drawTree(implicit sh: Show[A]) = draw.foldMap(_ + "\n")

  def draw(implicit sh: Show[A]): Stream[String] = {
    def drawSubTrees(s: Stream[Tree[A]]): Stream[String] = s match {
      case Stream.empty => Stream.empty
      case Stream(t) => Stream.cons("|", shift("`- ", "   ", t.draw))
      case Stream.cons(t, ts) => Stream.cons("|", shift("+- ", "|  ", t.draw)) append drawSubTrees(ts)
    }
    def shift(first: String, other: String, s: Stream[String]) =
      Stream.cons(first, other.repeat[Stream]).zipWith(((_: String) + (_: String)), s |!|)
    Stream.cons(rootLabel.shows, drawSubTrees(subForest))
  }

  def flatten = squish[A](Stream.empty)

  private def squish[AA >: A](xs: Stream[AA]): Stream[AA] =
    Stream.cons(rootLabel, subForest.foldr[Stream[AA]](xs, _.squish(_)))

  def levels: Stream[Stream[A]] = {
    val f = (s: Stream[Tree[A]]) => s.foldMap(_.subForest)
    val rl = (s: Stream[Tree[A]]) => s.map(_.rootLabel)
    Stream(this).iterate[Stream](f).takeWhile(!_.isEmpty).map(rl)
  }

  def cobind[B](f: Tree[A] => B): Tree[B] = this.unfoldTree((t: Tree[A]) => (f(t), () => t.subForest))

  def loc = TreeLoc.loc(this, Stream.empty, Stream.empty, Stream.empty)
}

object Tree {
  def node[A](root: A, forest: Stream[Tree[A]]) = new Tree[A] {
    val rootLabel = root
    def subForest = forest
  }
}