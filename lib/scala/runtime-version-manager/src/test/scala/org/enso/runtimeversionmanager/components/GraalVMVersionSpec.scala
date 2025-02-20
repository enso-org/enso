package org.enso.runtimeversionmanager.components

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GraalVMVersionSpec extends AnyWordSpec with Matchers {

  "GraalVM Version" should {
    "greater JDK version is considered newer" in {
      val graalVersion = "24.0.0"
      val ver1         = GraalVMVersion(graalVersion, "21.0.2")
      val ver2         = GraalVMVersion(graalVersion, "17.0.7")
      (ver1.compareTo(ver2) > 0) shouldBe true
    }

    "If JDK version is same (semver), greater Graal version is newer" in {
      val jdkVersion = "21.0.2"
      val ver1       = GraalVMVersion("24.0.2", jdkVersion)
      val ver2       = GraalVMVersion("24.0.0", jdkVersion)
      (ver1.compareTo(ver2) > 0) shouldBe true
    }

    "If JDK version is same (non semver), greater Graal version is newer" in {
      val jdkVersion = "21"
      val ver1       = GraalVMVersion("24.0.2", jdkVersion)
      val ver2       = GraalVMVersion("24.0.0", jdkVersion)
      (ver1.compareTo(ver2) > 0) shouldBe true
    }

    "be correctly ordered" in {
      val ver1       = GraalVMVersion("24.0.0", "21.0.2")
      val ver2       = GraalVMVersion("23.1.2", "21.0.2")
      val ver3       = GraalVMVersion("23.1.0", "21.0.1")
      val ver4       = GraalVMVersion("23.0.0", "17.0.7")
      val lst        = List(ver4, ver2, ver3, ver1)
      val sortedList = lst.sortWith((ver1, ver2) => ver1.compareTo(ver2) < 0)
      sortedList shouldEqual List(ver4, ver3, ver2, ver1)
    }
  }
}
