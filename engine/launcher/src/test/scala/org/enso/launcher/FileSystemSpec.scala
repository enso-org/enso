package org.enso.launcher

import java.nio.file.attribute.PosixFilePermissions

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.jdk.CollectionConverters._

class FileSystemSpec extends AnyWordSpec with Matchers {
  "decodePOSIXPermissions" should {
    def assertPair(octal: String, textual: String): Unit = {
      val mode      = Integer.parseInt(octal, 8)
      val decoded   = FileSystem.decodePOSIXPermissions(mode)
      val reference = PosixFilePermissions.fromString(textual)
      decoded.asScala.toSet shouldEqual reference.asScala.toSet
    }

    "decode permissions correctly" in {
      assertPair("000", "---------")
      assertPair("111", "--x--x--x")
      assertPair("222", "-w--w--w-")
      assertPair("444", "r--r--r--")
      assertPair("777", "rwxrwxrwx")

      assertPair("644", "rw-r--r--")
      assertPair("012", "-----x-w-")
      assertPair("421", "r---w---x")
    }
  }
}
