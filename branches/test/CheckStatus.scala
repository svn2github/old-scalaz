package scalaz.test

// todo this replaces Result, which is bogus (and everything depending on it i.e. Testable, Property)
sealed trait CheckStatus {
  import CheckStatus._
  
  def fold[X](proven: List[Argument[_]] => X,
              unfalsified: => X,
              falsified: List[Argument[_]] => X,
              undecided: => X,
              propertyException: (List[Argument[_]], Throwable) => X,
              genException: Throwable => X): X = this match {
    case Proven(args, _, _) => proven(args)
    case Unfalsified(_, _) => unfalsified
    case Falsified(args, _, _) => falsified(args)
    case Undecided(_, _) => undecided
    case PropertyException(args, ex, _, _) => propertyException(args, ex)
    case GenException(ex, _, _) => genException(ex)
  }

  def succeeded: Int = this match {
    case Proven(_, s, _) => s
    case Unfalsified(s, _) => s
    case Falsified(_, s, _) => s
    case Undecided(s, _) => s
    case PropertyException(_, _, s, _) => s
    case GenException(_, s, _) => s
  }

  def discardsd: Int = this match {
    case Proven(_, _, d) => d
    case Unfalsified(_, d) => d
    case Falsified(_, _, d) => d
    case Undecided(_, d) => d
    case PropertyException(_, _, _, d) => d
    case GenException(_, _, d) => d
  }
}

object CheckStatus {
  private final case class Proven(args: List[Argument[_]], s: Int, d: Int) extends CheckStatus
  private final case class Unfalsified(s: Int, d: Int) extends CheckStatus
  private final case class Falsified(args: List[Argument[_]], s: Int, d: Int) extends CheckStatus
  private final case class Undecided(s: Int, d: Int) extends CheckStatus
  private final case class PropertyException(args: List[Argument[_]], ex: Throwable, s: Int, d: Int) extends CheckStatus
  private final case class GenException(ex: Throwable, s: Int, d: Int) extends CheckStatus

  def proven(args: List[Argument[_]], s: Int, d: Int): CheckStatus = Proven(args, s, d)
  def unfalsified(s: Int, d: Int): CheckStatus = Unfalsified(s, d)
  def falsified(args: List[Argument[_]], s: Int, d: Int): CheckStatus = Falsified(args, s, d)
  def undecided(s: Int, d: Int): CheckStatus = Undecided(s, d)
  def propertyException(args: List[Argument[_]], ex: Throwable, s: Int, d: Int): CheckStatus = PropertyException(args, ex, s, d)
  def genException(ex: Throwable, s: Int, d: Int): CheckStatus = GenException(ex, s, d)
}