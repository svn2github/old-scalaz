// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * Wraps <code>scala.Predef.String</code> and provides additional methods.
 *
 * @see scala.Predef.String
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait StringW {
  /**
   * The value of this string.
   */
  val s: String

  /**
   * Parses this string as a boolean and returns it in <code>Right</code>.
   */
  val parseBoolean = EitherW.throws(java.lang.Boolean.parseBoolean(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a byte and returns it in <code>Right</code>.
   */
  val parseByte = EitherW.throws(java.lang.Byte.parseByte(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a double and returns it in <code>Right</code>.
   */
  val parseDouble = EitherW.throws(java.lang.Double.parseDouble(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a float and returns it in <code>Right</code>.
   */
  val parseFloat = EitherW.throws(java.lang.Float.parseFloat(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as an int and returns it in <code>Right</code>.
   */
  val parseInt = EitherW.throws(java.lang.Integer.parseInt(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a long and returns it in <code>Right</code>.
   */
  val parseLong = EitherW.throws(java.lang.Long.parseLong(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a short and returns it in <code>Right</code>.
   */
  val parseShort = EitherW.throws(java.lang.Short.parseShort(s)).left.map(_.asInstanceOf[NumberFormatException])
}

/**
 * Functions over strings.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object StringW {
  /**
   * Wraps a <code>scala.Predef.String</code>.
   */
  implicit def StringWString(s: StringW) = s.s

  /**
   * Unwraps a <code>scala.Predef.String</code>.
   */
  implicit def StringStringW(ss: String): StringW = new StringW {
    val s = ss
  }
}
