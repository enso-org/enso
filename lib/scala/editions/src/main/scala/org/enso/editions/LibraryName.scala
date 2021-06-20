package org.enso.editions

/** Represents a library name that should uniquely identify the library.
  *
  * The prefix is either a special prefix or a username.
  */
case class LibraryName(prefix: String, name: String) {

  /** The qualified name of the library consists of its prefix and name
    * separated with a dot.
    */
  def qualifiedName: String = s"$prefix.$name"

  /** @inheritdoc */
  override def toString: String = qualifiedName
}
