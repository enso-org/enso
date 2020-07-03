package org.enso.searcher.sql

import org.enso.searcher.Suggestion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class SuggestionsRepoTest
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfter
    with BeforeAndAfterAll {

  val Timeout: FiniteDuration = 10.seconds

  val db   = new SqlDatabase()
  val repo = new SqlSuggestionsRepo()

  override def beforeAll(): Unit = {
    Await.ready(db.run(repo.init), Timeout)
  }

  override def afterAll(): Unit = {
    db.close()
  }

  before {
    Await.ready(db.run(repo.clean), Timeout)
  }

  "SuggestionsRepo" should {

    "get all suggestions" in {
      val action =
        for {
          _   <- db.run(repo.insert(suggestion.atom))
          _   <- db.run(repo.insert(suggestion.method))
          _   <- db.run(repo.insert(suggestion.function))
          _   <- db.run(repo.insert(suggestion.local))
          all <- db.run(repo.getAll)
        } yield all

      val suggestions = Await.result(action, Timeout).map(_.suggestion)
      suggestions should contain theSameElementsAs Seq(
        suggestion.atom,
        suggestion.method,
        suggestion.function,
        suggestion.local
      )
    }

    "fail to insert duplicate suggestion" in {
      val action =
        for {
          id1 <- db.run(repo.insert(suggestion.atom))
          id2 <- db.run(repo.insert(suggestion.atom))
          _   <- db.run(repo.insert(suggestion.method))
          _   <- db.run(repo.insert(suggestion.method))
          _   <- db.run(repo.insert(suggestion.function))
          _   <- db.run(repo.insert(suggestion.function))
          _   <- db.run(repo.insert(suggestion.local))
          _   <- db.run(repo.insert(suggestion.local))
          all <- db.run(repo.getAll)
        } yield (id1, id2, all)

      val (id1, id2, all) = Await.result(action, Timeout)
      id1 shouldBe a[Some[_]]
      id2 shouldBe a[None.type]
      all.map(_.suggestion) should contain theSameElementsAs Seq(
        suggestion.atom,
        suggestion.method,
        suggestion.function,
        suggestion.local
      )
    }

    "fail to insertAll duplicate suggestion" in {
      val action =
        for {
          ids <- db.run(repo.insertAll(Seq(suggestion.local, suggestion.local)))
          all <- db.run(repo.getAll)
        } yield (ids, all)

      val (ids, all) = Await.result(action, Timeout)
      ids.flatten.length shouldEqual 1
      all.map(_.suggestion) should contain theSameElementsAs Seq(
        suggestion.local
      )
    }

    "select suggestion by id" in {
      val action =
        for {
          Some(id) <- db.run(repo.insert(suggestion.atom))
          res      <- db.run(repo.select(id))
        } yield res

      Await.result(action, Timeout) shouldEqual Some(suggestion.atom)
    }

    "remove suggestion" in {
      val action =
        for {
          id1 <- db.run(repo.insert(suggestion.atom))
          id2 <- db.run(repo.remove(suggestion.atom))
        } yield (id1, id2)

      val (id1, id2) = Await.result(action, Timeout)
      id1 shouldEqual id2
    }

    "get version" in {
      val action =
        db.run(repo.currentVersion)

      Await.result(action, Timeout) shouldEqual 0L
    }

    "change version after insert" in {
      val action = for {
        v1 <- db.run(repo.currentVersion)
        _  <- db.run(repo.insert(suggestion.atom))
        v2 <- db.run(repo.currentVersion)
      } yield (v1, v2)

      val (v1, v2) = Await.result(action, Timeout)
      v1 should not equal v2
    }

    "not change version after failed insert" in {
      val action = for {
        v1 <- db.run(repo.currentVersion)
        _  <- db.run(repo.insert(suggestion.atom))
        v2 <- db.run(repo.currentVersion)
        _  <- db.run(repo.insert(suggestion.atom))
        v3 <- db.run(repo.currentVersion)
      } yield (v1, v2, v3)

      val (v1, v2, v3) = Await.result(action, Timeout)
      v1 should not equal v2
      v2 shouldEqual v3
    }

    "change version after remove" in {
      val action = for {
        v1 <- db.run(repo.currentVersion)
        _  <- db.run(repo.insert(suggestion.local))
        v2 <- db.run(repo.currentVersion)
        _  <- db.run(repo.remove(suggestion.local))
        v3 <- db.run(repo.currentVersion)
      } yield (v1, v2, v3)

      val (v1, v2, v3) = Await.result(action, Timeout)
      v1 should not equal v2
      v2 should not equal v3
    }

    "not change version after failed remove" in {
      val action = for {
        v1 <- db.run(repo.currentVersion)
        _  <- db.run(repo.insert(suggestion.local))
        v2 <- db.run(repo.currentVersion)
        _  <- db.run(repo.remove(suggestion.local))
        v3 <- db.run(repo.currentVersion)
        _  <- db.run(repo.remove(suggestion.local))
        v4 <- db.run(repo.currentVersion)
      } yield (v1, v2, v3, v4)

      val (v1, v2, v3, v4) = Await.result(action, Timeout)
      v1 should not equal v2
      v2 should not equal v3
      v3 shouldEqual v4
    }

    "search suggestion by empty query" in {
      val action = for {
        _   <- db.run(repo.insert(suggestion.atom))
        _   <- db.run(repo.insert(suggestion.method))
        _   <- db.run(repo.insert(suggestion.function))
        _   <- db.run(repo.insert(suggestion.local))
        res <- db.run(repo.search(None, None, None))
      } yield res

      val res = Await.result(action, Timeout)
      res.isEmpty shouldEqual true
    }

    "search suggestion by self type" in {
      val action = for {
        _   <- db.run(repo.insert(suggestion.atom))
        id2 <- db.run(repo.insert(suggestion.method))
        _   <- db.run(repo.insert(suggestion.function))
        _   <- db.run(repo.insert(suggestion.local))
        res <- db.run(repo.search(Some("Main"), None, None))
      } yield (id2, res)

      val (id, res) = Await.result(action, Timeout)
      res should contain theSameElementsAs Seq(id).flatten
    }

    "search suggestion by return type" in {
      val action = for {
        _   <- db.run(repo.insert(suggestion.atom))
        _   <- db.run(repo.insert(suggestion.method))
        id3 <- db.run(repo.insert(suggestion.function))
        id4 <- db.run(repo.insert(suggestion.local))
        res <- db.run(repo.search(None, Some("MyType"), None))
      } yield (id3, id4, res)

      val (id1, id2, res) = Await.result(action, Timeout)
      res should contain theSameElementsAs Seq(id1, id2).flatten
    }

    "search suggestion by kind" in {
      val kinds = Seq(Suggestion.Kind.Atom, Suggestion.Kind.Local)
      val action = for {
        id1 <- db.run(repo.insert(suggestion.atom))
        _   <- db.run(repo.insert(suggestion.method))
        _   <- db.run(repo.insert(suggestion.function))
        id4 <- db.run(repo.insert(suggestion.local))
        res <- db.run(repo.search(None, None, Some(kinds)))
      } yield (id1, id4, res)

      val (id1, id2, res) = Await.result(action, Timeout)
      res should contain theSameElementsAs Seq(id1, id2).flatten
    }

    "search suggestion by empty kinds" in {
      val action = for {
        _   <- db.run(repo.insert(suggestion.atom))
        _   <- db.run(repo.insert(suggestion.method))
        _   <- db.run(repo.insert(suggestion.function))
        _   <- db.run(repo.insert(suggestion.local))
        res <- db.run(repo.search(None, None, Some(Seq())))
      } yield res

      val res = Await.result(action, Timeout)
      res.isEmpty shouldEqual true
    }

    "search suggestion by return type and kind" in {
      val kinds = Seq(Suggestion.Kind.Atom, Suggestion.Kind.Local)
      val action = for {
        _   <- db.run(repo.insert(suggestion.atom))
        _   <- db.run(repo.insert(suggestion.method))
        _   <- db.run(repo.insert(suggestion.function))
        id4 <- db.run(repo.insert(suggestion.local))
        res <- db.run(repo.search(None, Some("MyType"), Some(kinds)))
      } yield (id4, res)

      val (id, res) = Await.result(action, Timeout)
      res should contain theSameElementsAs Seq(id).flatten
    }

    "search suggestion by self and return types" in {
      val action = for {
        _   <- db.run(repo.insert(suggestion.atom))
        _   <- db.run(repo.insert(suggestion.method))
        _   <- db.run(repo.insert(suggestion.function))
        _   <- db.run(repo.insert(suggestion.local))
        res <- db.run(repo.search(Some("Main"), Some("MyType"), None))
      } yield res

      val res = Await.result(action, Timeout)
      res.isEmpty shouldEqual true
    }

    "search suggestion by all parameters" in {
      val kinds = Seq(
        Suggestion.Kind.Atom,
        Suggestion.Kind.Method,
        Suggestion.Kind.Function
      )
      val action = for {
        _   <- db.run(repo.insert(suggestion.atom))
        _   <- db.run(repo.insert(suggestion.method))
        _   <- db.run(repo.insert(suggestion.function))
        _   <- db.run(repo.insert(suggestion.local))
        res <- db.run(repo.search(Some("Main"), Some("MyType"), Some(kinds)))
      } yield res

      val res = Await.result(action, Timeout)
      res.isEmpty shouldEqual true
    }
  }

  object suggestion {

    val atom: Suggestion.Atom =
      Suggestion.Atom(
        name = "Pair",
        arguments = Seq(
          Suggestion.Argument("a", "Any", false, false, None),
          Suggestion.Argument("b", "Any", false, false, None)
        ),
        returnType    = "Pair",
        documentation = Some("Awesome")
      )

    val method: Suggestion.Method =
      Suggestion.Method(
        name          = "main",
        arguments     = Seq(),
        selfType      = "Main",
        returnType    = "IO",
        documentation = None
      )

    val function: Suggestion.Function =
      Suggestion.Function(
        name = "bar",
        arguments = Seq(
          Suggestion.Argument("x", "Number", false, true, Some("0"))
        ),
        returnType = "MyType",
        scope      = Suggestion.Scope(5, 9)
      )

    val local: Suggestion.Local =
      Suggestion.Local(
        name       = "bazz",
        returnType = "MyType",
        scope      = Suggestion.Scope(37, 84)
      )
  }
}
