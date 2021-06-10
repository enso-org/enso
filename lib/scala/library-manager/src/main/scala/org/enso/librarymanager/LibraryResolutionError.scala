package org.enso.librarymanager

case class LibraryResolutionError(message: String)
    extends RuntimeException(message)
