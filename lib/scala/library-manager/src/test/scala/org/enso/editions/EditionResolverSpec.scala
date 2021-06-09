package org.enso.editions

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EditionResolverSpec extends AnyWordSpec with Matchers {
  "EditionResolver" should {
    "resolve a simple edition" in                                       {}
    "resolve a nested edition" in                                       {}
    "correctly handle repository shadowing when resolving libraries" in {}
    "avoid cycles in the resolution" in                                 {}
  }
}
