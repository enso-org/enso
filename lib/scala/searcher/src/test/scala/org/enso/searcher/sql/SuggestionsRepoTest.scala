package org.enso.searcher.sql

import java.nio.file.{Files, Path}
import java.util.UUID

import org.enso.polyglot.Suggestion
import org.enso.testkit.RetrySpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class SuggestionsRepoTest extends AnyWordSpec with Matchers with RetrySpec {

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

  def withRepo(test: SqlSuggestionsRepo => Any): Any = {
    val tmpdb = Files.createTempFile(tmpdir, "suggestions-repo", ".db")
    val repo  = SqlSuggestionsRepo(tmpdb.toFile)
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

    "get all suggestions" taggedAs Retry in withRepo { repo =>
      val action =
        for {
          _   <- repo.insert(suggestion.atom)
          _   <- repo.insert(suggestion.method)
          _   <- repo.insert(suggestion.function)
          _   <- repo.insert(suggestion.local)
          all <- repo.getAll
        } yield all._2

      val suggestions = Await.result(action, Timeout).map(_.suggestion)
      suggestions should contain theSameElementsAs Seq(
        suggestion.atom,
        suggestion.method,
        suggestion.function,
        suggestion.local
      )
    }

    "get suggestions by method call info" taggedAs Retry in withRepo { repo =>
      val action = for {
        (_, ids) <- repo.insertAll(
          Seq(
            suggestion.atom,
            suggestion.method,
            suggestion.function,
            suggestion.local
          )
        )
        results <- repo.getAllMethods(
          Seq(("Test.Main", "Main", "main"), ("Test.Main", "Main", "foo"))
        )
      } yield (ids, results)

      val (ids, results) = Await.result(action, Timeout)
      results should contain theSameElementsInOrderAs Seq(ids(1), None)
    }

    "get suggestions by empty method call info" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          _ <- repo.insertAll(
            Seq(
              suggestion.atom,
              suggestion.method,
              suggestion.function,
              suggestion.local
            )
          )
          results <- repo.getAllMethods(Seq())
        } yield results

        val results = Await.result(action, Timeout)
        results.isEmpty shouldEqual true
    }

    "fail to insert duplicate suggestion" taggedAs Retry in withRepo { repo =>
      val action =
        for {
          (_, ids) <- repo.insertAll(
            Seq(
              suggestion.atom,
              suggestion.atom,
              suggestion.method,
              suggestion.method,
              suggestion.function,
              suggestion.function,
              suggestion.local,
              suggestion.local
            )
          )
          all <- repo.getAll
        } yield (ids, all._2)

      val (ids, all) = Await.result(action, Timeout)
      ids(0) shouldBe a[Some[_]]
      ids(1) shouldBe a[None.type]
      all.map(_.suggestion) should contain theSameElementsAs Seq(
        suggestion.atom,
        suggestion.method,
        suggestion.function,
        suggestion.local
      )
    }

    "fail to insertAll duplicate suggestion" taggedAs Retry in withRepo {
      repo =>
        val action =
          for {
            (v1, ids) <- repo.insertAll(Seq(suggestion.local, suggestion.local))
            (v2, all) <- repo.getAll
          } yield (v1, v2, ids, all)

        val (v1, v2, ids, all) = Await.result(action, Timeout)
        v1 shouldEqual v2
        ids.flatten.length shouldEqual 1
        all.map(_.suggestion) should contain theSameElementsAs Seq(
          suggestion.local
        )
    }

    "select suggestion by id" taggedAs Retry in withRepo { repo =>
      val action =
        for {
          Some(id) <- repo.insert(suggestion.atom)
          res      <- repo.select(id)
        } yield res

      Await.result(action, Timeout) shouldEqual Some(suggestion.atom)
    }

    "remove suggestion" taggedAs Retry in withRepo { repo =>
      val action =
        for {
          id1 <- repo.insert(suggestion.atom)
          id2 <- repo.remove(suggestion.atom)
        } yield (id1, id2)

      val (id1, id2) = Await.result(action, Timeout)
      id1 shouldEqual id2
    }

    "remove suggestions by module name" taggedAs Retry in withRepo { repo =>
      val action = for {
        id1      <- repo.insert(suggestion.atom)
        id2      <- repo.insert(suggestion.method)
        id3      <- repo.insert(suggestion.function)
        id4      <- repo.insert(suggestion.local)
        (_, ids) <- repo.removeByModule(suggestion.atom.module)
      } yield (Seq(id1, id2, id3, id4).flatten, ids)

      val (inserted, removed) = Await.result(action, Timeout)
      inserted should contain theSameElementsAs removed
    }

    "remove all suggestions" taggedAs Retry in withRepo { repo =>
      val action = for {
        id1      <- repo.insert(suggestion.atom)
        _        <- repo.insert(suggestion.method)
        _        <- repo.insert(suggestion.function)
        id4      <- repo.insert(suggestion.local)
        (_, ids) <- repo.removeAll(Seq(suggestion.atom, suggestion.local))
      } yield (Seq(id1, id4), ids)

      val (inserted, removed) = Await.result(action, Timeout)
      inserted should contain theSameElementsAs removed
    }

    "get version" taggedAs Retry in withRepo { repo =>
      val action = repo.currentVersion

      Await.result(action, Timeout) shouldEqual 0L
    }

    "change version after insert" taggedAs Retry in withRepo { repo =>
      val action = for {
        v1 <- repo.currentVersion
        _  <- repo.insert(suggestion.atom)
        v2 <- repo.currentVersion
      } yield (v1, v2)

      val (v1, v2) = Await.result(action, Timeout)
      v1 should not equal v2
    }

    "not change version after failed insert" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          v1 <- repo.currentVersion
          _  <- repo.insert(suggestion.atom)
          v2 <- repo.currentVersion
          _  <- repo.insert(suggestion.atom)
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
          (v3, _) <- repo.removeByModule(suggestion.local.module)
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
          _       <- repo.removeByModule(suggestion.local.module)
          v3      <- repo.currentVersion
          (v4, _) <- repo.removeByModule(suggestion.local.module)
        } yield (v1, v2, v3, v4)

        val (v1, v2, v3, v4) = Await.result(action, Timeout)
        v1 should not equal v2
        v2 should not equal v3
        v3 shouldEqual v4
    }

    "change version after remove all suggestions" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          v1      <- repo.currentVersion
          _       <- repo.insert(suggestion.local)
          v2      <- repo.currentVersion
          (v3, _) <- repo.removeAll(Seq(suggestion.local))
        } yield (v1, v2, v3)

        val (v1, v2, v3) = Await.result(action, Timeout)
        v1 should not equal v2
        v2 should not equal v3
    }

    "not change version after failed remove all suggestions" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          v1      <- repo.currentVersion
          _       <- repo.insert(suggestion.local)
          v2      <- repo.currentVersion
          (v3, _) <- repo.removeAll(Seq(suggestion.local))
          (v4, _) <- repo.removeAll(Seq(suggestion.local))
        } yield (v1, v2, v3, v4)

        val (v1, v2, v3, v4) = Await.result(action, Timeout)
        v1 should not equal v2
        v2 should not equal v3
        v3 shouldEqual v4
    }

    "update suggestion by external id" taggedAs Retry in withRepo { repo =>
      val newReturnType = "Quux"
      val action = for {
        _         <- repo.insert(suggestion.atom)
        _         <- repo.insert(suggestion.method)
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

    "change version after updateAll" taggedAs Retry in withRepo { repo =>
      val newReturnType = "Quux"
      val action = for {
        _   <- repo.insert(suggestion.atom)
        _   <- repo.insert(suggestion.method)
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
          _   <- repo.insert(suggestion.atom)
          _   <- repo.insert(suggestion.method)
          _   <- repo.insert(suggestion.function)
          _   <- repo.insert(suggestion.local)
          v1  <- repo.currentVersion
          res <- repo.updateAll(Seq(UUID.randomUUID() -> newReturnType))
        } yield (res._2, v1, res._1)

        val (updatedIds, v1, v2) = Await.result(action, Timeout)
        updatedIds shouldEqual Seq(None)
        v1 shouldEqual v2
    }

    "rename the module" taggedAs Retry in withRepo { repo =>
      val newModuleName = "Best.Main"
      val action = for {
        _ <- repo.insertAll(
          Seq(
            suggestion.atom,
            suggestion.method,
            suggestion.function,
            suggestion.local
          )
        )
        _        <- repo.renameProject("Test", "Best")
        (_, res) <- repo.getAll
      } yield res

      val res = Await.result(action, Timeout)
      res.map(_.suggestion) should contain theSameElementsAs Seq(
        suggestion.atom.copy(module     = newModuleName),
        suggestion.method.copy(module   = newModuleName),
        suggestion.function.copy(module = newModuleName),
        suggestion.local.copy(module    = newModuleName)
      )
    }

    "not change version after renaming the module" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          v1 <- repo.insert(suggestion.atom)
          _  <- repo.renameProject("Test", "Zest")
          v2 <- repo.currentVersion
        } yield (v1, v2)

        val (v1, v2) = Await.result(action, Timeout)
        v1 shouldEqual Some(v2)
    }

    "search suggestion by empty query" taggedAs Retry in withRepo { repo =>
      val action = for {
        _   <- repo.insert(suggestion.atom)
        _   <- repo.insert(suggestion.method)
        _   <- repo.insert(suggestion.function)
        _   <- repo.insert(suggestion.local)
        res <- repo.search(None, None, None, None, None)
      } yield res._2

      val res = Await.result(action, Timeout)
      res.isEmpty shouldEqual true
    }

    "search suggestion by module" taggedAs Retry in withRepo { repo =>
      val action = for {
        id1 <- repo.insert(suggestion.atom)
        id2 <- repo.insert(suggestion.method)
        id3 <- repo.insert(suggestion.function)
        id4 <- repo.insert(suggestion.local)
        res <- repo.search(Some("Test.Main"), None, None, None, None)
      } yield (id1, id2, id3, id4, res._2)

      val (id1, id2, id3, id4, res) = Await.result(action, Timeout)
      res should contain theSameElementsAs Seq(id1, id2, id3, id4).flatten
    }

    "search suggestion by empty module" taggedAs Retry in withRepo { repo =>
      val action = for {
        _   <- repo.insert(suggestion.atom)
        _   <- repo.insert(suggestion.method)
        _   <- repo.insert(suggestion.function)
        _   <- repo.insert(suggestion.local)
        res <- repo.search(Some(""), None, None, None, None)
      } yield res._2

      val res = Await.result(action, Timeout)
      res.isEmpty shouldEqual true
    }

    "search suggestion by self type" taggedAs Retry in withRepo { repo =>
      val action = for {
        _   <- repo.insert(suggestion.atom)
        id2 <- repo.insert(suggestion.method)
        _   <- repo.insert(suggestion.function)
        _   <- repo.insert(suggestion.local)
        res <- repo.search(None, Some("Main"), None, None, None)
      } yield (id2, res._2)

      val (id, res) = Await.result(action, Timeout)
      res should contain theSameElementsAs Seq(id).flatten
    }

    "search suggestion by return type" taggedAs Retry in withRepo { repo =>
      val action = for {
        _   <- repo.insert(suggestion.atom)
        _   <- repo.insert(suggestion.method)
        id3 <- repo.insert(suggestion.function)
        id4 <- repo.insert(suggestion.local)
        res <- repo.search(None, None, Some("MyType"), None, None)
      } yield (id3, id4, res._2)

      val (id1, id2, res) = Await.result(action, Timeout)
      res should contain theSameElementsAs Seq(id1, id2).flatten
    }

    "search suggestion by kind" taggedAs Retry in withRepo { repo =>
      val kinds = Seq(Suggestion.Kind.Atom, Suggestion.Kind.Local)
      val action = for {
        id1 <- repo.insert(suggestion.atom)
        _   <- repo.insert(suggestion.method)
        _   <- repo.insert(suggestion.function)
        id4 <- repo.insert(suggestion.local)
        res <- repo.search(None, None, None, Some(kinds), None)
      } yield (id1, id4, res._2)

      val (id1, id2, res) = Await.result(action, Timeout)
      res should contain theSameElementsAs Seq(id1, id2).flatten
    }

    "search suggestion by empty kinds" taggedAs Retry in withRepo { repo =>
      val action = for {
        _   <- repo.insert(suggestion.atom)
        _   <- repo.insert(suggestion.method)
        _   <- repo.insert(suggestion.function)
        _   <- repo.insert(suggestion.local)
        res <- repo.search(None, None, None, Some(Seq()), None)
      } yield res._2

      val res = Await.result(action, Timeout)
      res.isEmpty shouldEqual true
    }

    "search suggestion global by scope" taggedAs Retry in withRepo { repo =>
      val action = for {
        id1 <- repo.insert(suggestion.atom)
        id2 <- repo.insert(suggestion.method)
        _   <- repo.insert(suggestion.function)
        _   <- repo.insert(suggestion.local)
        res <-
          repo.search(None, None, None, None, Some(Suggestion.Position(99, 42)))
      } yield (id1, id2, res._2)

      val (id1, id2, res) = Await.result(action, Timeout)
      res should contain theSameElementsAs Seq(id1, id2).flatten
    }

    "search suggestion local by scope" taggedAs Retry in withRepo { repo =>
      val action = for {
        id1 <- repo.insert(suggestion.atom)
        id2 <- repo.insert(suggestion.method)
        id3 <- repo.insert(suggestion.function)
        _   <- repo.insert(suggestion.local)
        res <-
          repo.search(None, None, None, None, Some(Suggestion.Position(1, 5)))
      } yield (id1, id2, id3, res._2)

      val (id1, id2, id3, res) = Await.result(action, Timeout)
      res should contain theSameElementsAs Seq(id1, id2, id3).flatten
    }

    "search suggestion by module and self type" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          _   <- repo.insert(suggestion.atom)
          id2 <- repo.insert(suggestion.method)
          _   <- repo.insert(suggestion.function)
          _   <- repo.insert(suggestion.local)
          res <- repo.search(Some("Test.Main"), Some("Main"), None, None, None)
        } yield (id2, res._2)

        val (id, res) = Await.result(action, Timeout)
        res should contain theSameElementsAs Seq(id).flatten
    }

    "search suggestion by return type and kind" taggedAs Retry in withRepo {
      repo =>
        val kinds = Seq(Suggestion.Kind.Atom, Suggestion.Kind.Local)
        val action = for {
          _   <- repo.insert(suggestion.atom)
          _   <- repo.insert(suggestion.method)
          _   <- repo.insert(suggestion.function)
          id4 <- repo.insert(suggestion.local)
          res <- repo.search(None, None, Some("MyType"), Some(kinds), None)
        } yield (id4, res._2)

        val (id, res) = Await.result(action, Timeout)
        res should contain theSameElementsAs Seq(id).flatten
    }

    "search suggestion by return type and scope" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          _   <- repo.insert(suggestion.atom)
          _   <- repo.insert(suggestion.method)
          _   <- repo.insert(suggestion.function)
          id4 <- repo.insert(suggestion.local)
          res <- repo.search(
            None,
            None,
            Some("MyType"),
            None,
            Some(Suggestion.Position(42, 0))
          )
        } yield (id4, res._2)

        val (id, res) = Await.result(action, Timeout)
        res should contain theSameElementsAs Seq(id).flatten
    }

    "search suggestion by kind and scope" taggedAs Retry in withRepo { repo =>
      val kinds = Seq(Suggestion.Kind.Atom, Suggestion.Kind.Local)
      val action = for {
        id1 <- repo.insert(suggestion.atom)
        _   <- repo.insert(suggestion.method)
        _   <- repo.insert(suggestion.function)
        _   <- repo.insert(suggestion.local)
        res <- repo.search(
          None,
          None,
          None,
          Some(kinds),
          Some(Suggestion.Position(99, 1))
        )
      } yield (id1, res._2)

      val (id, res) = Await.result(action, Timeout)
      res should contain theSameElementsAs Seq(id).flatten
    }

    "search suggestion by self and return types" taggedAs Retry in withRepo {
      repo =>
        val action = for {
          _   <- repo.insert(suggestion.atom)
          _   <- repo.insert(suggestion.method)
          _   <- repo.insert(suggestion.function)
          _   <- repo.insert(suggestion.local)
          res <- repo.search(None, Some("Main"), Some("MyType"), None, None)
        } yield res._2

        val res = Await.result(action, Timeout)
        res.isEmpty shouldEqual true
    }

    "search suggestion by module, return type and kind" taggedAs Retry in withRepo {
      repo =>
        val kinds = Seq(Suggestion.Kind.Atom, Suggestion.Kind.Local)
        val action = for {
          _   <- repo.insert(suggestion.atom)
          _   <- repo.insert(suggestion.method)
          _   <- repo.insert(suggestion.function)
          id4 <- repo.insert(suggestion.local)
          res <- repo.search(
            Some("Test.Main"),
            None,
            Some("MyType"),
            Some(kinds),
            None
          )
        } yield (id4, res._2)

        val (id, res) = Await.result(action, Timeout)
        res should contain theSameElementsAs Seq(id).flatten
    }

    "search suggestion by return type, kind and scope" taggedAs Retry in withRepo {
      repo =>
        val kinds = Seq(Suggestion.Kind.Atom, Suggestion.Kind.Local)
        val action = for {
          _   <- repo.insert(suggestion.atom)
          _   <- repo.insert(suggestion.method)
          _   <- repo.insert(suggestion.function)
          id4 <- repo.insert(suggestion.local)
          res <- repo.search(
            None,
            None,
            Some("MyType"),
            Some(kinds),
            Some(Suggestion.Position(42, 0))
          )
        } yield (id4, res._2)

        val (id, res) = Await.result(action, Timeout)
        res should contain theSameElementsAs Seq(id).flatten
    }

    "search suggestion by all parameters" taggedAs Retry in withRepo { repo =>
      val kinds = Seq(
        Suggestion.Kind.Atom,
        Suggestion.Kind.Method,
        Suggestion.Kind.Function
      )
      val action = for {
        _ <- repo.insert(suggestion.atom)
        _ <- repo.insert(suggestion.method)
        _ <- repo.insert(suggestion.function)
        _ <- repo.insert(suggestion.local)
        res <- repo.search(
          Some("Test.Main"),
          Some("Main"),
          Some("MyType"),
          Some(kinds),
          Some(Suggestion.Position(42, 0))
        )
      } yield res._2

      val res = Await.result(action, Timeout)
      res.isEmpty shouldEqual true
    }
  }

  object suggestion {

    val atom: Suggestion.Atom =
      Suggestion.Atom(
        externalId = None,
        module     = "Test.Main",
        name       = "Pair",
        arguments = Seq(
          Suggestion.Argument("a", "Any", false, false, None),
          Suggestion.Argument("b", "Any", false, false, None)
        ),
        returnType    = "Pair",
        documentation = Some("Awesome")
      )

    val method: Suggestion.Method =
      Suggestion.Method(
        externalId    = Some(UUID.randomUUID()),
        module        = "Test.Main",
        name          = "main",
        arguments     = Seq(),
        selfType      = "Main",
        returnType    = "IO",
        documentation = None
      )

    val function: Suggestion.Function =
      Suggestion.Function(
        externalId = Some(UUID.randomUUID()),
        module     = "Test.Main",
        name       = "bar",
        arguments = Seq(
          Suggestion.Argument("x", "Number", false, true, Some("0"))
        ),
        returnType = "MyType",
        scope =
          Suggestion.Scope(Suggestion.Position(1, 5), Suggestion.Position(1, 9))
      )

    val local: Suggestion.Local =
      Suggestion.Local(
        externalId = Some(UUID.randomUUID()),
        module     = "Test.Main",
        name       = "bazz",
        returnType = "MyType",
        scope = Suggestion.Scope(
          Suggestion.Position(32, 0),
          Suggestion.Position(84, 0)
        )
      )
  }
}
