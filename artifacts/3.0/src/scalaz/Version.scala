// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * Prints the version of this software.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision<br>
 *          $LastChangedDate<br>
 *          $LastChangedBy$
 */
object Version extends Application {
  println("Scalaz version TEST")
  println("Compiled against Scala version 2.7.1.r15535-b20080712185233")
  println("Tested using Reductio version %reductio.version%")
  println("Copyright 2008 Workingmouse Pty. Ltd.")
}
