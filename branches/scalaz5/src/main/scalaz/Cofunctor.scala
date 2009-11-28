package scalaz

trait Cofunctor[F[_]] {
  def comap[A, B](r: F[A], f: B => A): F[B]
}

object Cofunctor {
  import Scalaz._
  
  implicit def Function1Cofunctor[X]: Cofunctor[PartialApply1Of2[Function1, X]#Flip] = new Cofunctor[PartialApply1Of2[Function1, X]#Flip] {
    def comap[A, B](r: A => X, f: B => A) = r compose f
  }

  implicit val MetricSpaceCofunctor: Cofunctor[MetricSpace] = new Cofunctor[MetricSpace] {
    def comap[A, B](r: MetricSpace[A], f: B => A) = metricSpace[B]((b1, b2) => r distance (f(b1), f(b2)))
  }
}
