package org.enso.searcher.sql

import org.enso.polyglot.{ExportedSymbol, ModuleExports, Suggestion}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.searcher.SuggestionEntry
import org.enso.searcher.data.QueryResult
import org.enso.searcher.memory.InmemorySuggestionsRepo
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

  def withRepo(test: InmemorySuggestionsRepo => Any): Any = {
    val repo = new InmemorySuggestionsRepo()
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

    "select suggestion by id" taggedAs Retry in withRepo { repo =>
      val action =
        for {
          Some(id) <- repo.insert(suggestion.constructor)
          res      <- repo.select(id)
        } yield res

      Await.result(action, Timeout) shouldEqual Some(suggestion.constructor)
    }

    "remove suggestion" taggedAs Retry in withRepo { repo =>
      val action =
        for {
          id1 <- repo.insert(suggestion.constructor)
          id2 <- repo.remove(suggestion.constructor)
        } yield (id1, id2)

      val (id1, id2) = Await.result(action, Timeout)
      id1 shouldEqual id2
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

    "change version after remove" taggedAs Retry in withRepo { repo =>
      val action = for {
        v1 <- repo.currentVersion
        _  <- repo.insert(suggestion.local)
        v2 <- repo.currentVersion
        _  <- repo.remove(suggestion.local)
        v3 <- repo.currentVersion
      } yield (v1, v2, v3)

      val (v1, v2, v3) = Await.result(action, Timeout)
      v1 should not equal v2
      v2 should not equal v3
    }

    "not change version after failed remove" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          v1 <- repo.currentVersion
          _  <- repo.insert(suggestion.local)
          v2 <- repo.currentVersion
          _  <- repo.remove(suggestion.local)
          v3 <- repo.currentVersion
          _  <- repo.remove(suggestion.local)
          v4 <- repo.currentVersion
        } yield (v1, v2, v3, v4)

        val (v1, v2, v3, v4) = Await.result(action, Timeout)
        v1 should not equal v2
        v2 should not equal v3
        v3 shouldEqual v4
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

    "update suggestion by external id" taggedAs Retry in withRepo { repo =>
      val newReturnType = "Quux"
      val action = for {
        _         <- repo.insert(suggestion.module)
        _         <- repo.insert(suggestion.tpe)
        _         <- repo.insert(suggestion.constructor)
        _         <- repo.insert(suggestion.method)
        _         <- repo.insert(suggestion.conversion)
        _         <- repo.insert(suggestion.function)
        Some(id4) <- repo.insert(suggestion.local)
        res <-
          repo.updateAll(Seq(suggestion.local.externalId.get -> newReturnType))
        Some(val4) <- repo.select(id4)
      } yield (id4, res._2, val4)

      val (suggestionId, updatedIds, result) = Await.result(action, Timeout)
      updatedIds.flatten shouldEqual Seq(suggestionId)
      result shouldEqual suggestion.local.copy(returnType = newReturnType)
    }

    "update suggestion external id" taggedAs Retry in withRepo { repo =>
      val newUuid = UUID.randomUUID()
      val action = for {
        (v1, Seq(_, _, _, id1, _, _, _)) <- repo.insertAll(
          Seq(
            suggestion.module,
            suggestion.tpe,
            suggestion.constructor,
            suggestion.method,
            suggestion.conversion,
            suggestion.function,
            suggestion.local
          )
        )
        (v2, Some(id2)) <- repo.update(
          suggestion.method,
          Some(Some(newUuid)),
          None,
          None,
          None
        )
        s <- repo.select(id1)
      } yield (v1, id1, v2, id2, s)
      val (v1, id1, v2, id2, s) = Await.result(action, Timeout)
      v1 should not equal v2
      id1 shouldEqual id2
      s shouldEqual Some(suggestion.method.copy(externalId = Some(newUuid)))
    }

    "update suggestion removing external id" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          (v1, Seq(_, _, _, _, _, id1, _)) <- repo.insertAll(
            Seq(
              suggestion.module,
              suggestion.tpe,
              suggestion.constructor,
              suggestion.method,
              suggestion.conversion,
              suggestion.function,
              suggestion.local
            )
          )
          (v2, Some(id2)) <- repo.update(
            suggestion.function,
            Some(None),
            None,
            None,
            None
          )
          s <- repo.select(id1)
        } yield (v1, id1, v2, id2, s)
        val (v1, id1, v2, id2, s) = Await.result(action, Timeout)
        v1 should not equal v2
        id1 shouldEqual id2
        s shouldEqual Some(suggestion.function.copy(externalId = None))
    }

    "update suggestion return type" taggedAs Retry in withRepo { repo =>
      val newReturnType = "NewType"
      val action = for {
        (v1, Seq(_, _, _, _, _, id1, _)) <- repo.insertAll(
          Seq(
            suggestion.module,
            suggestion.tpe,
            suggestion.constructor,
            suggestion.method,
            suggestion.conversion,
            suggestion.function,
            suggestion.local
          )
        )
        (v2, Some(id2)) <- repo.update(
          suggestion.function,
          None,
          Some(newReturnType),
          None,
          None
        )
        s <- repo.select(id1)
      } yield (v1, id1, v2, id2, s)
      val (v1, id1, v2, id2, s) = Await.result(action, Timeout)
      v1 should not equal v2
      id1 shouldEqual id2
      s shouldEqual Some(suggestion.function.copy(returnType = newReturnType))
    }

    "update suggestion type documentation" taggedAs Retry in withRepo { repo =>
      val newDoc = "My Doc"
      val action = for {
        (v1, Seq(_, id1, _, _, _, _, _)) <- repo.insertAll(
          Seq(
            suggestion.module,
            suggestion.tpe,
            suggestion.constructor,
            suggestion.method,
            suggestion.conversion,
            suggestion.function,
            suggestion.local
          )
        )
        (v2, Some(id2)) <- repo.update(
          suggestion.tpe,
          None,
          None,
          Some(Some(newDoc)),
          None
        )
        s <- repo.select(id1)
      } yield (v1, id1, v2, id2, s)
      val (v1, id1, v2, id2, s) = Await.result(action, Timeout)
      v1 should not equal v2
      id1 shouldEqual id2
      s shouldEqual Some(
        suggestion.tpe.copy(documentation = Some(newDoc))
      )
    }

    "update suggestion constructor documentation" taggedAs Retry in withRepo {
      repo =>
        val newDoc = "My Doc"
        val action = for {
          (v1, Seq(_, _, id1, _, _, _, _)) <- repo.insertAll(
            Seq(
              suggestion.module,
              suggestion.tpe,
              suggestion.constructor,
              suggestion.method,
              suggestion.conversion,
              suggestion.function,
              suggestion.local
            )
          )
          (v2, Some(id2)) <- repo.update(
            suggestion.constructor,
            None,
            None,
            Some(Some(newDoc)),
            None
          )
          s <- repo.select(id1)
        } yield (v1, id1, v2, id2, s)
        val (v1, id1, v2, id2, s) = Await.result(action, Timeout)
        v1 should not equal v2
        id1 shouldEqual id2
        s shouldEqual Some(
          suggestion.constructor.copy(documentation = Some(newDoc))
        )
    }

    "update suggestion module documentation" taggedAs Retry in withRepo {
      repo =>
        val newDoc = "My Doc"
        val action = for {
          (v1, Seq(id1, _, _, _, _, _, _)) <- repo.insertAll(
            Seq(
              suggestion.module,
              suggestion.tpe,
              suggestion.constructor,
              suggestion.method,
              suggestion.conversion,
              suggestion.function,
              suggestion.local
            )
          )
          (v2, Some(id2)) <- repo.update(
            suggestion.module,
            None,
            None,
            Some(Some(newDoc)),
            None
          )
          s <- repo.select(id1)
        } yield (v1, id1, v2, id2, s)
        val (v1, id1, v2, id2, s) = Await.result(action, Timeout)
        v1 should not equal v2
        id1 shouldEqual id2
        s shouldEqual Some(suggestion.module.copy(documentation = Some(newDoc)))
    }

    "update suggestion conversion documentation" taggedAs Retry in withRepo {
      repo =>
        val newDoc = "My Doc"
        val action = for {
          (v1, Seq(_, _, _, _, id1, _, _)) <- repo.insertAll(
            Seq(
              suggestion.module,
              suggestion.tpe,
              suggestion.constructor,
              suggestion.method,
              suggestion.conversion,
              suggestion.function,
              suggestion.local
            )
          )
          (v2, Some(id2)) <- repo.update(
            suggestion.conversion,
            None,
            None,
            Some(Some(newDoc)),
            None
          )
          s <- repo.select(id1)
        } yield (v1, id1, v2, id2, s)
        val (v1, id1, v2, id2, s) = Await.result(action, Timeout)
        v1 should not equal v2
        id1 shouldEqual id2
        s shouldEqual Some(
          suggestion.conversion.copy(documentation = Some(newDoc))
        )
    }

    "update suggestion function documentation" taggedAs Retry in withRepo {
      repo =>
        val newDoc = "My awesome function!"
        val action = for {
          (v1, Seq(_, _, _, _, _, id1, _)) <- repo.insertAll(
            Seq(
              suggestion.module,
              suggestion.tpe,
              suggestion.constructor,
              suggestion.method,
              suggestion.conversion,
              suggestion.function,
              suggestion.local
            )
          )
          (v2, Some(id2)) <- repo.update(
            suggestion.function,
            None,
            None,
            Some(Some(newDoc)),
            None
          )
          s <- repo.select(id1)
        } yield (v1, id1, v2, id2, s)
        val (v1, id1, v2, id2, s) = Await.result(action, Timeout)
        v1 should not equal v2
        id1 shouldEqual id2
        s shouldEqual Some(
          suggestion.function.copy(documentation = Some(newDoc))
        )
    }

    "update suggestion local documentation" taggedAs Retry in withRepo { repo =>
      val newDoc = "Some stuff there"
      val action = for {
        (v1, Seq(_, _, _, _, _, _, id1)) <- repo.insertAll(
          Seq(
            suggestion.module,
            suggestion.tpe,
            suggestion.constructor,
            suggestion.method,
            suggestion.conversion,
            suggestion.function,
            suggestion.local
          )
        )
        (v2, Some(id2)) <- repo.update(
          suggestion.local,
          None,
          None,
          Some(Some(newDoc)),
          None
        )
        s <- repo.select(id1)
      } yield (v1, id1, v2, id2, s)
      val (v1, id1, v2, id2, s) = Await.result(action, Timeout)
      v1 should not equal v2
      id1 shouldEqual id2
      s shouldEqual Some(
        suggestion.local.copy(documentation = Some(newDoc))
      )
    }

    "update suggestion removing documentation" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          (v1, Seq(_, _, id1, _, _, _, _)) <- repo.insertAll(
            Seq(
              suggestion.module,
              suggestion.tpe,
              suggestion.constructor,
              suggestion.method,
              suggestion.conversion,
              suggestion.function,
              suggestion.local
            )
          )
          (v2, Some(id2)) <- repo.update(
            suggestion.constructor,
            None,
            None,
            Some(None),
            None
          )
          s <- repo.select(id1)
        } yield (v1, id1, v2, id2, s)
        val (v1, id1, v2, id2, s) = Await.result(action, Timeout)
        v1 should not equal v2
        id1 shouldEqual id2
        s shouldEqual Some(suggestion.constructor.copy(documentation = None))
    }

    "update suggestion scope" taggedAs Retry in withRepo { repo =>
      val newScope = Suggestion.Scope(
        Suggestion.Position(14, 15),
        Suggestion.Position(42, 43)
      )
      val action = for {
        (v1, Seq(_, _, _, _, _, _, id1)) <- repo.insertAll(
          Seq(
            suggestion.module,
            suggestion.tpe,
            suggestion.constructor,
            suggestion.method,
            suggestion.conversion,
            suggestion.function,
            suggestion.local
          )
        )
        (v2, Some(id2)) <- repo.update(
          suggestion.local,
          None,
          None,
          None,
          Some(newScope)
        )
        s <- repo.select(id1)
      } yield (v1, id1, v2, id2, s)
      val (v1, id1, v2, id2, s) = Await.result(action, Timeout)
      v1 should not equal v2
      id1 shouldEqual id2
      s shouldEqual Some(suggestion.local.copy(scope = newScope))
    }

    "update suggestion empty request" taggedAs Retry in withRepo { repo =>
      val action = for {
        (v1, Seq(_, _, _, id1, _, _, _)) <- repo.insertAll(
          Seq(
            suggestion.module,
            suggestion.tpe,
            suggestion.constructor,
            suggestion.method,
            suggestion.conversion,
            suggestion.function,
            suggestion.local
          )
        )
        (v2, id2) <- repo.update(
          suggestion.method,
          None,
          None,
          None,
          None
        )
      } yield (v1, v2, id1, id2)
      val (v1, v2, id1, id2) = Await.result(action, Timeout)
      v1 shouldEqual v2
      id2 shouldEqual Some(id1)
    }

    "change version after updateAll" taggedAs Retry in withRepo { repo =>
      val newReturnType = "Quux"
      val action = for {
        _   <- repo.insert(suggestion.module)
        _   <- repo.insert(suggestion.tpe)
        _   <- repo.insert(suggestion.constructor)
        _   <- repo.insert(suggestion.method)
        _   <- repo.insert(suggestion.conversion)
        _   <- repo.insert(suggestion.function)
        id4 <- repo.insert(suggestion.local)
        v1  <- repo.currentVersion
        res <-
          repo.updateAll(Seq(suggestion.local.externalId.get -> newReturnType))
      } yield (id4, res._2, v1, res._1)

      val (suggestionId, updatedIds, v1, v2) = Await.result(action, Timeout)
      updatedIds shouldEqual Seq(suggestionId)
      v1 should not equal v2
    }

    "not change version after failed updateAll" taggedAs Retry in withRepo {
      repo =>
        val newReturnType = "Quux"
        val action = for {
          _   <- repo.insert(suggestion.module)
          _   <- repo.insert(suggestion.tpe)
          _   <- repo.insert(suggestion.constructor)
          _   <- repo.insert(suggestion.method)
          _   <- repo.insert(suggestion.conversion)
          _   <- repo.insert(suggestion.function)
          _   <- repo.insert(suggestion.local)
          v1  <- repo.currentVersion
          res <- repo.updateAll(Seq(UUID.randomUUID() -> newReturnType))
        } yield (res._2, v1, res._1)

        val (updatedIds, v1, v2) = Await.result(action, Timeout)
        updatedIds shouldEqual Seq(None)
        v1 shouldEqual v2
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
        externalId    = Some(UUID.randomUUID()),
        module        = "local.Test.Main",
        arguments     = Seq(),
        selfType      = "local.Test.Main.Foo",
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
