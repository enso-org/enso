package org.enso.librarymanager.local

import org.enso.editions.LibraryName
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Path

class LocalLibraryProviderSpec extends AnyWordSpec with Matchers with Inside {
  private val primaryPath: Path =
    Path.of(getClass.getResource("test-library-path").toURI)
  private val secondaryPath: Path =
    Path.of(getClass.getResource("secondary-test-library-path").toURI)

  val libraryPath: List[Path] = List(primaryPath, secondaryPath)

  "LocalLibraryProvider" should {
    "resolve local libraries by config name regardless of directory name" in {
      val provider = new DefaultLocalLibraryProvider(libraryPath)
      inside(provider.findLibrary(LibraryName("user1", "Library_1"))) {
        case Some(root) =>
          root.location shouldEqual secondaryPath.resolve("new_folder1")
      }
    }

    "resolve local libraries with conflicting names, disambiguating by namespace" in {
      val provider = new DefaultLocalLibraryProvider(libraryPath)
      inside(provider.findLibrary(LibraryName("dev1", "Simple_Library"))) {
        case Some(root) =>
          root.location shouldEqual primaryPath.resolve("Simple_Library_1")
      }
    }

    "pick alphabetically first path if there are multiple ambiguous libraries with the same name+namespace in a single directory" in {
      // Ideally this should bubble-up to be a compiler warning/error in the
      // import statement, but it's a lot of additional work for a rare
      // occurrence, so for now we'll just log a warning - we can revisit this
      // later.
      val provider = new DefaultLocalLibraryProvider(libraryPath)
      val ambiguousName =
        LibraryName("ambiguous_developer", "Ambiguous_Library")
      inside(provider.findLibrary(ambiguousName)) { case Some(root) =>
        val expectedPath = primaryPath.resolve("ambiguous1")
        root.location shouldEqual expectedPath
      }
    }

    "prefer libraries in the first directory in the search path" in {
      val provider = new DefaultLocalLibraryProvider(libraryPath)
      inside(
        provider.findLibrary(LibraryName("user123", "Library_In_Both_Dirs"))
      ) { case Some(root) =>
        root.location shouldEqual primaryPath.resolve("library_in_both_dirs")
      }
    }
  }
}
