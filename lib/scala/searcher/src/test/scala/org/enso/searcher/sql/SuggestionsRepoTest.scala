package org.enso.searcher.sql

import org.enso.polyglot.{ExportedSymbol, ModuleExports, Suggestion}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.searcher.SuggestionEntry
import org.enso.searcher.data.QueryResult
import org.enso.searcher.memory.InMemorySuggestionsRepo
import org.enso.searcher.sql.equality.SuggestionsEquality
import org.enso.testkit.RetrySpec
import org.scalactic.TripleEqualsSupport
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}
import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class SuggestionsRepoTest
    extends AnyWordSpec
    with Matchers
    with RetrySpec
    with TripleEqualsSupport
    with SuggestionsEquality {

  val Timeout: FiniteDuration = 20.seconds

  val tmpdir: Path = {
    val tmp = Files.createTempDirectory("suggestions-repo-test")
    sys.addShutdownHook {
      Files.list(tmp).forEach { path =>
        path.toFile.delete()
      }
      tmp.toFile.delete()
    }
    tmp
  }

  def withRepo(test: InMemorySuggestionsRepo => Any): Any = {
    val repo = new InMemorySuggestionsRepo()
    Await.ready(repo.init, Timeout)
    try test(repo)
    finally {
      Await.ready(repo.clean, Timeout)
      repo.close()
    }
  }

  "SuggestionsRepo" should {

    "init idempotent" taggedAs Retry in withRepo { repo =>
      Await.result(repo.init, Timeout)
    }

    "insert all suggestions" taggedAs Retry in withRepo { repo =>
      val action =
        for {
          v1        <- repo.currentVersion
          (v2, ids) <- repo.insertAll(suggestion.all)
          all       <- repo.getAll
        } yield (ids, all._2, v1, v2)

      val (ids, entries, v1, v2) = Await.result(action, Timeout)
      val expectedEntries        = ids.zip(suggestion.all).map(SuggestionEntry.tupled)
      entries should contain theSameElementsAs expectedEntries
      v1 should not equal v2
    }

    "get all suggestions" taggedAs Retry in withRepo { repo =>
      val action =
        for {
          _   <- repo.insertAll(suggestion.all)
          all <- repo.getAll
        } yield all._2

      val suggestions = Await.result(action, Timeout).map(_.suggestion)
      suggestions should contain theSameElementsAs suggestion.all
    }

    "fail to insertAll duplicate suggestion" taggedAs Retry in withRepo {
      repo =>
        val action =
          for {
            _ <- repo.insertAll(Seq(suggestion.local, suggestion.local))
          } yield ()

        an[RuntimeException] should be thrownBy Await.result(
          action,
          Timeout
        )
    }

    "remove suggestions by module names" taggedAs Retry in withRepo { repo =>
      val action = for {
        (_, idsIns) <- repo.insertAll(suggestion.all)
        (_, idsRem) <- repo.removeModules(Seq(suggestion.constructor.module))
      } yield (idsIns, idsRem)

      val (inserted, removed) = Await.result(action, Timeout)
      inserted should contain theSameElementsAs removed
    }

    "remove suggestions by empty module names" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          (v1, _)       <- repo.insertAll(suggestion.all)
          (v2, removed) <- repo.removeModules(Seq())
        } yield (v1, v2, removed)

        val (v1, v2, removed) = Await.result(action, Timeout)
        v1 shouldEqual v2
        removed shouldEqual Seq()
    }

    "get version" taggedAs Retry in withRepo { repo =>
      val action = repo.currentVersion

      Await.result(action, Timeout) shouldEqual 0L
    }

    "change version after insert" taggedAs Retry in withRepo { repo =>
      val action = for {
        v1 <- repo.currentVersion
        _  <- repo.insert(suggestion.constructor)
        v2 <- repo.currentVersion
      } yield (v1, v2)

      val (v1, v2) = Await.result(action, Timeout)
      v1 should not equal v2
    }

    "not change version after failed insert" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          v1 <- repo.currentVersion
          _  <- repo.insert(suggestion.constructor)
          v2 <- repo.currentVersion
          _  <- repo.insert(suggestion.constructor)
          v3 <- repo.currentVersion
        } yield (v1, v2, v3)

        val (v1, v2, v3) = Await.result(action, Timeout)
        v1 should not equal v2
        v2 shouldEqual v3
    }

    "change version after remove by module name" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          v1      <- repo.currentVersion
          _       <- repo.insert(suggestion.local)
          v2      <- repo.currentVersion
          (v3, _) <- repo.removeModules(Seq(suggestion.local.module))
        } yield (v1, v2, v3)

        val (v1, v2, v3) = Await.result(action, Timeout)
        v1 should not equal v2
        v2 should not equal v3
    }

    "not change version after failed remove by module name" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          v1      <- repo.currentVersion
          _       <- repo.insert(suggestion.local)
          v2      <- repo.currentVersion
          _       <- repo.removeModules(Seq(suggestion.local.module))
          v3      <- repo.currentVersion
          (v4, _) <- repo.removeModules(Seq(suggestion.local.module))
        } yield (v1, v2, v3, v4)

        val (v1, v2, v3, v4) = Await.result(action, Timeout)
        v1 should not equal v2
        v2 should not equal v3
        v3 shouldEqual v4
    }

    "get exported symbols" taggedAs Retry in withRepo { repo =>
      val reexport = "Foo.Bar"
      val method   = suggestion.method.copy(reexports = Set(reexport))
      val updates = Seq(
        Api.ExportsUpdate(
          ModuleExports(
            reexport,
            Set(ExportedSymbol.Module(suggestion.module.module))
          ),
          Api.ExportsAction.Add()
        ),
        Api.ExportsUpdate(
          ModuleExports(
            reexport,
            Set(ExportedSymbol.Method(method.module, method.name))
          ),
          Api.ExportsAction.Remove()
        )
      )
      val action = for {
        (_, ids) <- repo.insertAll(
          Seq(
            suggestion.module,
            suggestion.tpe,
            suggestion.constructor,
            method,
            suggestion.conversion,
            suggestion.function,
            suggestion.local
          )
        )
        results <- repo.getExportedSymbols(updates)
      } yield (ids, results)

      val (ids, results) = Await.result(action, Timeout)
      results should contain theSameElementsAs Seq(
        QueryResult(Seq(ids(0)), updates(0)),
        QueryResult(Seq(ids(3)), updates(1))
      )
    }
  }

  object suggestion {

    val module: Suggestion.Module =
      Suggestion.Module(
        module        = "local.Test.Main",
        documentation = Some("This is a main module.")
      )

    val tpe: Suggestion.Type =
      Suggestion.Type(
        externalId = None,
        module     = "local.Test.Main",
        name       = "Maybe",
        params = Seq(
          Suggestion.Argument("a", "Any", false, false, None)
        ),
        returnType    = "Standard.Builtins.Maybe",
        parentType    = Some("Standard.Builtins.Any"),
        documentation = Some("To be or not to be")
      )

    val constructor: Suggestion.Constructor =
      Suggestion.Constructor(
        externalId = None,
        module     = "local.Test.Main",
        name       = "Standard.Builtins.Pair",
        arguments = Seq(
          Suggestion.Argument("a", "Any", false, false, None),
          Suggestion.Argument("b", "Any", false, false, None)
        ),
        returnType    = "Standard.Builtins.Pair",
        documentation = Some("Awesome"),
        annotations   = Seq()
      )

    val getter: Suggestion.Getter =
      Suggestion.Getter(
        externalId    = Some(UUID.randomUUID()),
        module        = "local.Test.Main",
        name          = "a",
        arguments     = Seq(),
        selfType      = "Standard.Builtins.Pair",
        returnType    = "Standard.Builtins.IO",
        documentation = None,
        annotations   = Seq()
      )

    val method: Suggestion.DefinedMethod =
      Suggestion.DefinedMethod(
        externalId    = Some(UUID.randomUUID()),
        module        = "local.Test.Main",
        name          = "main",
        arguments     = Seq(),
        selfType      = "local.Test.Main",
        returnType    = "Standard.Builtins.IO",
        isStatic      = true,
        documentation = None,
        annotations   = Seq()
      )

    val instanceMethod: Suggestion.DefinedMethod =
      Suggestion.DefinedMethod(
        externalId    = Some(UUID.randomUUID()),
        module        = "local.Test.Main",
        name          = "foo",
        arguments     = Seq(),
        selfType      = "local.Test.Main.A",
        returnType    = "Standard.Builtins.Nothing",
        isStatic      = false,
        documentation = None,
        annotations   = Seq()
      )

    val conversion: Suggestion.Conversion =
      Suggestion.Conversion(
        externalId = Some(UUID.randomUUID()),
        module     = "local.Test.Main",
        arguments = Seq(
          Suggestion.Argument("that", "local.Test.Main.Foo", false, false, None)
        ),
        selfType      = "local.Test.Main.Bar",
        returnType    = "local.Test.Main.Bar",
        documentation = None
      )

    val function: Suggestion.Function =
      Suggestion.Function(
        externalId = Some(UUID.randomUUID()),
        module     = "local.Test.Main",
        name       = "bar",
        arguments = Seq(
          Suggestion.Argument("x", "Number", false, true, Some("0"))
        ),
        returnType = "local.Test.Main.MyType",
        scope = Suggestion
          .Scope(Suggestion.Position(1, 5), Suggestion.Position(6, 0)),
        documentation = Some("My function bar.")
      )

    val local: Suggestion.Local =
      Suggestion.Local(
        externalId = Some(UUID.randomUUID()),
        module     = "local.Test.Main",
        name       = "bazz",
        returnType = "local.Test.Main.MyType",
        scope = Suggestion.Scope(
          Suggestion.Position(3, 4),
          Suggestion.Position(6, 0)
        ),
        documentation = Some("Some bazz")
      )

    val all: Seq[Suggestion] = Seq(
      module,
      tpe,
      constructor,
      getter,
      method,
      instanceMethod,
      conversion,
      function,
      local
    )

  }
}
