package scalaz

object Example {
  def run {
    ExampleBifunctor.run
    ExampleCategory.run
    ExampleCofunctor.run
    ExampleFunctor.run
    ExampleKleisli.run
  }
  
  def main(args: Array[String]) = run
}
