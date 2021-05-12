package org.enso.logger.masking

import java.nio.file.Path

import org.enso.logger.masking.MaskingUtils.{fileSeparator, toMaskedPath, STUB}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MaskingUtilsSpec extends AnyWordSpec with Matchers {

  "MaskingUtils" should {

    "mask a path" in {
      toMaskedPath(Path.of("/")) shouldEqual STUB
      toMaskedPath(Path.of("/foo")) shouldEqual STUB
      toMaskedPath(Path.of("/foo/bar")) shouldEqual masked("bar")
      toMaskedPath(Path.of("/foo/bar.jar")) shouldEqual masked("bar.jar")
      toMaskedPath(Path.of("/foo/bar/baz")) shouldEqual masked("baz")
      toMaskedPath(Path.of("/foo/bar/baz.xyz")) shouldEqual masked("baz.xyz")
    }
  }

  private def masked(suffix: String): String =
    s"$STUB$fileSeparator$suffix"
}
