package org.enso.librarymanager

/** Indicates an error of library resolution. */
case class LibraryResolutionError(message: String)
    extends RuntimeException(message)
