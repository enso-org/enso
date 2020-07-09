package org.enso.pkg

/**
  * Represents a qualified name of a source module.
  *
  * @param path the names of the package and directories the module is
  *             contained in
  * @param module the name of the module
  */
case class QualifiedName(path: List[String], module: String) {
  override def toString: String =
    (path :+ module).mkString(QualifiedName.separator)

  /**
    * Get the parent of this qualified name.
    *
    * @return the parent of this qualified name.
    */
  def getParent: Option[QualifiedName] =
    path.lastOption.map(QualifiedName(path.init, _))

  /**
    * Create a child qualified name taking this name as a parent.
    *
    * @param name the name of a child node.
    * @return a new qualified name based on this name.
    */
  def createChild(name: String): QualifiedName =
    QualifiedName(path :+ module, name)

  /**
    * Renames a project part of this [[QualifiedName]].
    *
    * @param oldName the old project name
    * @param newName the new project name
    * @return a [[QualifiedName]] with the updated project name
    */
  def renameProject(oldName: String, newName: String): QualifiedName = {
    if (path.head == oldName) {
      this.copy(path = newName :: path.tail)
    } else {
      this
    }
  }

}

object QualifiedName {
  val separator      = "."
  val separatorRegex = "\\."

  /**
    * Parses a dot-separated string representation of a qualified name into
    * a [[QualifiedName]] object.
    *
    * @param qualName the string representation of a qualified name.
    * @return the corresponding [[QualifiedName]] object.
    */
  def fromString(qualName: String): Option[QualifiedName] = {
    val segments = qualName.split(separatorRegex).toList
    if (segments.nonEmpty) {
      Some(QualifiedName(segments.dropRight(1), segments.last))
    } else {
      None
    }
  }

  /**
    * Creates a qualified name with empty path.
    *
    * @param modName the module name.
    * @return a qualified name equivalent to `modName`
    */
  def simpleName(modName: String): QualifiedName =
    QualifiedName(List(), modName)
}
