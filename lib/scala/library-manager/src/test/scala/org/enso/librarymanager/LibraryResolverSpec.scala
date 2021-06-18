package org.enso.librarymanager

import nl.gn0s1s.bump.SemVer
import org.enso.editions.Editions.Repository
import org.enso.editions.{DefaultEnsoVersion, Editions, LibraryName}
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.testkit.EitherValue
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Path

class LibraryResolverSpec
    extends AnyWordSpec
    with Matchers
    with EitherValue
    with Inside {
  "LibraryResolver" should {
    val mainRepo = Repository.make("main", "https://example.com/main").get
    val parentEdition = Editions.Resolved.Edition(
      parent        = None,
      engineVersion = Some(DefaultEnsoVersion),
      repositories  = Map("main" -> mainRepo),
      libraries = Map(
        "Standard.Base" -> Editions.Resolved
          .PublishedLibrary("Standard.Base", SemVer(4, 5, 6), mainRepo)
      )
    )
    val customRepo = Repository.make("custom", "https://example.com/custom").get
    val currentEdition = Editions.Resolved.Edition(
      parent        = Some(parentEdition),
      engineVersion = None,
      repositories  = Map("custom" -> customRepo),
      libraries = Map(
        "Foo.Main" -> Editions.Resolved
          .PublishedLibrary("Foo.Main", SemVer(1, 0, 0), mainRepo),
        "Foo.My" -> Editions.Resolved
          .PublishedLibrary("Foo.My", SemVer(2, 0, 0), customRepo),
        "Foo.Local" -> Editions.Resolved.LocalLibrary("Foo.Local")
      )
    )

    case class FakeLocalLibraryProvider(fixtures: Map[String, Path])
        extends LocalLibraryProvider {
      override def findLibrary(libraryName: LibraryName): Option[Path] =
        fixtures.get(libraryName.qualifiedName)
    }

    val localLibraries = Map(
      "Foo.My"        -> Path.of("./Foo/My"),
      "Foo.Local"     -> Path.of("./Foo/Local"),
      "Standard.Base" -> Path.of("./Standard/Base")
    )

    val resolver = LibraryResolver(FakeLocalLibraryProvider(localLibraries))

    "correctly resolve libraries based on the edition" in {
      resolver
        .resolveLibraryVersion(
          libraryName          = LibraryName("Standard", "Base"),
          edition              = currentEdition,
          preferLocalLibraries = false
        )
        .rightValue shouldEqual
      LibraryVersion.Published(SemVer(4, 5, 6), mainRepo)

      resolver
        .resolveLibraryVersion(
          libraryName          = LibraryName("Foo", "Main"),
          edition              = currentEdition,
          preferLocalLibraries = false
        )
        .rightValue shouldEqual
      LibraryVersion.Published(SemVer(1, 0, 0), mainRepo)

      resolver
        .resolveLibraryVersion(
          libraryName          = LibraryName("Foo", "My"),
          edition              = currentEdition,
          preferLocalLibraries = false
        )
        .rightValue shouldEqual
      LibraryVersion.Published(SemVer(2, 0, 0), customRepo)

      resolver
        .resolveLibraryVersion(
          libraryName          = LibraryName("Foo", "Local"),
          edition              = currentEdition,
          preferLocalLibraries = false
        )
        .rightValue shouldEqual
      LibraryVersion.Local
    }

    "correctly handle preference of local libraries" in {
      resolver
        .resolveLibraryVersion(
          libraryName          = LibraryName("Standard", "Base"),
          edition              = currentEdition,
          preferLocalLibraries = true
        )
        .rightValue shouldEqual
      LibraryVersion.Local

      resolver
        .resolveLibraryVersion(
          libraryName          = LibraryName("Foo", "Main"),
          edition              = currentEdition,
          preferLocalLibraries = true
        )
        .rightValue shouldEqual
      LibraryVersion.Published(SemVer(1, 0, 0), mainRepo)

      resolver
        .resolveLibraryVersion(
          libraryName          = LibraryName("Foo", "My"),
          edition              = currentEdition,
          preferLocalLibraries = true
        )
        .rightValue shouldEqual
      LibraryVersion.Local

      resolver
        .resolveLibraryVersion(
          libraryName          = LibraryName("Foo", "Local"),
          edition              = currentEdition,
          preferLocalLibraries = true
        )
        .rightValue shouldEqual
      LibraryVersion.Local
    }

    "not fall back to a local library if it was not defined as such " +
    "explicitly nor `prefer-local-libraries` is set" in {
      val result = resolver.resolveLibraryVersion(
        libraryName          = LibraryName("Foo", "Local"),
        edition              = parentEdition,
        preferLocalLibraries = false
      )
      inside(result) { case Left(error) =>
        error.getMessage should include("not defined")
      }
    }
  }
}
