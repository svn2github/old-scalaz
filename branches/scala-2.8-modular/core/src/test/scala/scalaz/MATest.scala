package scalaz

import Scalaz._

object MATest {
  List(1): MA[List, Int]
  (Some(1): Option[Int]): MA[Option, Int]
  Stream(1): MA[Stream, Int]
}