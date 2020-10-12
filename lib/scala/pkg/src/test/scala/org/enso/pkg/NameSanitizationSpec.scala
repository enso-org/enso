package org.enso.pkg

import java.io.File

import org.enso.filesystem.FileSystem
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NameSanitizationSpec extends AnyWordSpec with Matchers {
  "Creating a new project" should {
    "sanitize the name of the project" in {
      implicit val fileSystem: FileSystem[File] = FileSystem.Default

      val manager = new PackageManager()

      manager.normalizeName("My_Project") shouldEqual "My_Project"
      manager.normalizeName("My___Project") shouldEqual "My_Project"
      manager.normalizeName("myProject") shouldEqual "My_Project"
      manager.normalizeName("myPro??^ject123") shouldEqual "My_Project_123"
      manager.normalizeName("???%$6543lib") shouldEqual "Project_6543_Lib"
    }
  }
}
