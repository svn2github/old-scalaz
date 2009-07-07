package scalaz.scalacheck

object Monad {
  import scalaz.Applicative._
  import Applicative._
  import scalaz.Bind._
  import Bind._
  import scalaz.Pointed._
  import Pointed._
  import scalaz.Monad._
  import Monad._
  import Pure._
  
  import org.scalacheck.{Gen, Arbitrary}

  implicit val GenMonad = monad[Gen]

  implicit val ArbitraryMonad = monad[Arbitrary]
}
