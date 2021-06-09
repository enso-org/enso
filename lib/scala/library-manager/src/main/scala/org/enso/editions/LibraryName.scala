package org.enso.editions

case class LibraryName(prefix: String, name: String) {
  def qualifiedName: String = s"$prefix.$name"
}
