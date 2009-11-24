package scalaz

import Scalaz._

object MMATest {
  List(List(1)): MMA[List, Int]
  Some(Some(1)): MMA[Option, Int]
  Stream(Stream(1)): MMA[Stream, Int]
}