// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database.sql

/**
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Select {
  def from: From
  def selectors: List[Selector]
  def sql = selectors match {
    case Nil => "*"
    case _ => selectors.map(_.toSQL).mkString(",")
  }

  def where(p: Predicate) = Database.query("SELECT " + sql + " FROM " + from.sql + " WHERE " + p.toSQL)
}

/**
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */  
object Select {
  def select(f: From, s: List[Selector]) = new Select {
    def from = f
    def selectors = s
  }

  implicit def SelectDatabaseResultSet(s: Select) = Database.query("SELECT " + s.sql + " FROM " + s.from.sql)
}
