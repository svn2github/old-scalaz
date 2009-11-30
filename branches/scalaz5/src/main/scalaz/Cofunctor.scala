package scalaz

trait Cofunctor[F[_]] {
  def comap[A, B](r: F[A], f: B => A): F[B]
}

object Cofunctor {
  import Scalaz._
  
  implicit def Function1Cofunctor[X]: Cofunctor[PartialApply1Of2[Function1, X]#Flip] = new Cofunctor[PartialApply1Of2[Function1, X]#Flip] {
    def comap[A, B](r: A => X, f: B => A) = r compose f
  }

  implicit val EqualCofunctor: Cofunctor[Equal] = new Cofunctor[Equal] {
    def comap[A, B](r: Equal[A], f: B => A) = equal[B]((b1, b2) => r equal (f(b1), f(b2)))
  }

  implicit val OrderCofunctor: Cofunctor[Order] = new Cofunctor[Order] {
    def comap[A, B](r: Order[A], f: B => A) = order[B]((b1, b2) => r order (f(b1), f(b2)))
  }

  implicit val ShowCofunctor: Cofunctor[Show] = new Cofunctor[Show] {
    def comap[A, B](r: Show[A], f: B => A) = show[B](b => r show (f(b)))
  }

  implicit val MetricSpaceCofunctor: Cofunctor[MetricSpace] = new Cofunctor[MetricSpace] {
    def comap[A, B](r: MetricSpace[A], f: B => A) = metricSpace[B]((b1, b2) => r distance (f(b1), f(b2)))
  }

  import concurrent.{Actor, Effect}

  implicit val ActorCofunctor: Cofunctor[Actor] = new Cofunctor[Actor] {
    def comap[A, B](r: Actor[A], f: B => A): Actor[B] = actor[B](r.onError, (b: B) => (r ! f(b))())(r.strategy)
  }

  implicit val EffectCofunctor: Cofunctor[Effect] = new Cofunctor[Effect] {
    def comap[A, B](r: Effect[A], f: B => A) = effect[B]((b) => r ! f(b))(r.strategy)
  }
}
