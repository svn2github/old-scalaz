package scalaz

object Example {
  def run {
    error("here")
    ExampleApplicative.run
    ExampleBifunctor.run
    ExampleCategory.run
    ExampleCofunctor.run
    ExampleFunctor.run
    ExampleKleisli.run
    ExampleMonad.run
    ExampleTraverse.run
  }
  
  def main(args: Array[String]) = run
}
