package org.enso.editions

import nl.gn0s1s.bump.SemVer
import org.enso.editions.EditionResolutionError.EditionResolutionCycle
import org.enso.editions.Editions.Repository
import org.enso.editions.provider.EditionProvider
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{Inside, OptionValues}

import scala.util.{Failure, Success, Try}

class EditionResolverSpec
    extends AnyWordSpec
    with Matchers
    with Inside
    with OptionValues {
  object FakeEditionProvider extends EditionProvider {
    val mainRepo = Repository.make("main", "https://example.com/main").get
    val editions: Map[String, Editions.RawEdition] = Map(
      "2021.0" -> Editions.Raw.Edition(
        parent        = None,
        engineVersion = Some(SemVerEnsoVersion(SemVer(1, 2, 3))),
        repositories = Map(
          "main" -> mainRepo
        ),
        libraries = Map(
          "Standard.Base" -> Editions.Raw
            .PublishedLibrary("Standard.Base", SemVer(1, 2, 3), "main")
        )
      ),
      "cycleA" -> Editions.Raw.Edition(
        parent        = Some("cycleB"),
        engineVersion = Some(SemVerEnsoVersion(SemVer(1, 2, 3))),
        repositories  = Map(),
        libraries     = Map()
      ),
      "cycleB" -> Editions.Raw.Edition(
        parent        = Some("cycleA"),
        engineVersion = Some(SemVerEnsoVersion(SemVer(1, 2, 3))),
        repositories  = Map(),
        libraries     = Map()
      )
    )

    override def findEditionForName(name: String): Try[Editions.Raw.Edition] =
      editions
        .get(name)
        .map(Success(_))
        .getOrElse(
          Failure(new RuntimeException(s"Could not load edition `$name`."))
        )
  }

  val resolver = EditionResolver(FakeEditionProvider)

  "EditionResolver" should {
    "resolve a simple edition" in {
      val repo = Repository.make("foo", "http://example.com").get
      val edition = Editions.Raw.Edition(
        parent        = None,
        engineVersion = Some(SemVerEnsoVersion(SemVer(1, 2, 3))),
        repositories  = Map("foo" -> repo),
        libraries = Map(
          "bar.baz" -> Editions.Raw.LocalLibrary("bar.baz"),
          "foo.bar" -> Editions.Raw
            .PublishedLibrary("foo.bar", SemVer(1, 2, 3), "foo")
        )
      )

      inside(resolver.resolve(edition)) { case Right(resolved) =>
        resolved.engineVersion shouldEqual edition.engineVersion
        resolved.parent should be(empty)
        resolved.repositories shouldEqual edition.repositories
        resolved.libraries should have size 2
        resolved.libraries("bar.baz") shouldEqual Editions.Resolved
          .LocalLibrary("bar.baz")
        resolved.libraries("foo.bar") shouldEqual Editions.Resolved
          .PublishedLibrary("foo.bar", SemVer(1, 2, 3), repo)
      }
    }

    "resolve a nested edition" in {
      val edition = Editions.Raw.Edition(
        parent        = Some("2021.0"),
        engineVersion = Some(SemVerEnsoVersion(SemVer(1, 2, 3))),
        repositories  = Map(),
        libraries = Map(
          "bar.baz" -> Editions.Raw.LocalLibrary("bar.baz"),
          "foo.bar" -> Editions.Raw
            .PublishedLibrary("foo.bar", SemVer(1, 2, 3), "main")
        )
      )

      inside(resolver.resolve(edition)) { case Right(resolved) =>
        resolved.parent should be(defined)
        resolved.libraries should have size 2
        resolved.libraries("bar.baz") shouldEqual Editions.Resolved
          .LocalLibrary("bar.baz")
        resolved.libraries("foo.bar") shouldEqual Editions.Resolved
          .PublishedLibrary(
            "foo.bar",
            SemVer(1, 2, 3),
            FakeEditionProvider.mainRepo
          )
      }
    }

    "correctly handle repository shadowing when resolving libraries" in {
      val localRepo = Repository.make("main", "http://example.com/local").get

      localRepo should not equal FakeEditionProvider.mainRepo

      val edition = Editions.Raw.Edition(
        parent        = Some("2021.0"),
        engineVersion = None,
        repositories  = Map("main" -> localRepo),
        libraries = Map(
          "foo.bar" -> Editions.Raw
            .PublishedLibrary("foo.bar", SemVer(1, 2, 3), "main")
        )
      )

      inside(resolver.resolve(edition)) { case Right(resolved) =>
        resolved.parent should be(defined)
        resolved.libraries should have size 1
        resolved.libraries("foo.bar") shouldEqual
        Editions.Resolved.PublishedLibrary(
          "foo.bar",
          SemVer(1, 2, 3),
          localRepo
        )

        resolved.parent.value.libraries("Standard.Base") shouldEqual
        Editions.Resolved.PublishedLibrary(
          "Standard.Base",
          SemVer(1, 2, 3),
          FakeEditionProvider.mainRepo
        )
      }
    }

    "avoid cycles in the resolution" in {
      val edition = Editions.Raw.Edition(
        parent        = Some("cycleA"),
        engineVersion = Some(SemVerEnsoVersion(SemVer(1, 2, 3))),
        repositories  = Map(),
        libraries     = Map()
      )

      val failure = resolver.resolve(edition)
      inside(failure) { case Left(EditionResolutionCycle(editions)) =>
        editions shouldEqual Seq("cycleA", "cycleB", "cycleA")
      }
    }
  }
}
