package org.enso.librarymanager

import nl.gn0s1s.bump.SemVer
import org.enso.editions.Editions.Repository

import java.nio.file.Path

sealed trait LibraryVersion
case class LocalVersion(path: Path) extends LibraryVersion
case class PublishedVersion(version: SemVer, recommendedRepository: Repository)
    extends LibraryVersion
