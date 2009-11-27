package scalaz

object Example {
  def run {
    ExampleFunctor.run
    ExampleCofunctor.run
  }
  
  def main(args: Array[String]) = run
}
