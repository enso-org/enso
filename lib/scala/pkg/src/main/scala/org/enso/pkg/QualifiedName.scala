package org.enso.pkg
import java.util

/** Represents a qualified name of a source item.
  *
  * @param path the names of the package and directories the item is
  *             contained in
  */
case class QualifiedName(path: Array[String]) {

  override def toString: String =
    path.mkString(QualifiedName.separator)

  override def equals(o: Any): Boolean = {
    o match {
      case that: QualifiedName =>
        util.Arrays.equals(
          path.asInstanceOf[Array[AnyRef]],
          that.path.asInstanceOf[Array[AnyRef]]
        )
      case _ =>
        false
    }
  }

  override def hashCode: Int =
    util.Arrays.hashCode(path.asInstanceOf[Array[AnyRef]])

  /** The name of the item. */
  def item: String = path.last

  /** Get the parent of this qualified name.
    *
    * @return the parent of this qualified name.
    */
  def getParent: Option[QualifiedName] = {
    if (path.isEmpty) None
    else Some(copy(path = path.init))
  }

  /** Create a child qualified name taking this name as a parent.
    *
    * @param name the name of a child node.
    * @return a new qualified name based on this name.
    */
  def createChild(name: String): QualifiedName =
    copy(path = path :+ name)

  /** Renames a project part of this [[QualifiedName]].
    *
    * @param newName the new project name
    * @return a [[QualifiedName]] with the updated project name
    */
  def renameProject(newName: String): QualifiedName = {
    val newPath = path.clone()
    newPath.update(0, newName)
    copy(path = newPath)
  }

}

object QualifiedName {
  val separator      = "."
  val separatorRegex = "\\."

  /** Parses a dot-separated string representation of a qualified name into
    * a [[QualifiedName]] object.
    *
    * @param qualName the string representation of a qualified name.
    * @return the corresponding [[QualifiedName]] object.
    */
  def fromString(qualName: String): QualifiedName = {
    val segments = qualName.split(separatorRegex)
    QualifiedName(segments)
  }

  /** Creates a qualified name with empty path.
    *
    * @param modName the module name.
    * @return a qualified name equivalent to `modName`
    */
  def simpleName(modName: String): QualifiedName =
    QualifiedName(Array(modName))
}
