package org.enso.semver

/** Implicits that should be imported when attempting to order Enso's versions.
  */
object SemVerOrdering {
  implicit object VersionOrdering extends Ordering[SemVer] {
    def compare(x: SemVer, y: SemVer): Int = x.compareTo(y)
  }
}
