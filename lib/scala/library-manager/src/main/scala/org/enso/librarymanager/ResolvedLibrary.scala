package org.enso.librarymanager

import org.enso.editions.{LibraryName, LibraryVersion}

import java.nio.file.Path

/** Represents a resolved library that is located somewhere on the filesystem. */
case class ResolvedLibrary(
  name: LibraryName,
  version: LibraryVersion,
  location: Path
)
