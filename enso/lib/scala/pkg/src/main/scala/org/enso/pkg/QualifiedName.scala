package org.enso.pkg

import scala.jdk.CollectionConverters._;

/** Represents a qualified name of a source item.
  *
  * @param path the names of the package and directories the item is
  *             contained in
  * @param item the name of the item
  */
case class QualifiedName(path: List[String], item: String) {
  override def toString: String =
    (path :+ item).mkString(QualifiedName.separator)

  /** Get the parent of this qualified name.
    *
    * @return the parent of this qualified name.
    */
  def getParent: Option[QualifiedName] =
    path.lastOption.map(QualifiedName(path.init, _))

  /** Create a child qualified name taking this name as a parent.
    *
    * @param name the name of a child node.
    * @return a new qualified name based on this name.
    */
  def createChild(name: String): QualifiedName =
    QualifiedName(path :+ item, name)

  /** Renames a project part of this [[QualifiedName]].
    *
    * @param newName the new project name
    * @return a [[QualifiedName]] with the updated project name
    */
  def renameProject(newName: String): QualifiedName = {
    val namespace = path.head
    this.copy(path = namespace :: newName :: path.drop(2))
  }

  /** Gets the path portion of the qualified name as a Java list.
   *
   * @return a Java list representation of the path portion of the qualified name.
   */
  def pathAsJava(): java.util.List[String] = {
    path.asJava
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
    val segments = qualName.split(separatorRegex).toList
    QualifiedName(segments.dropRight(1), segments.last)
  }

  /** Creates a qualified name with empty path.
    *
    * @param modName the module name.
    * @return a qualified name equivalent to `modName`
    */
  def simpleName(modName: String): QualifiedName =
    QualifiedName(List(), modName)
}
