package org.enso.searcher.sql

import org.enso.jsonrpc.test.RetrySpec
import org.enso.searcher.Suggestion
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class SuggestionsRepoTest
    extends AnyWordSpec
    with RetrySpec
    with Matchers
    with BeforeAndAfterAll {

  val Timeout: FiniteDuration = 3.seconds

  val db   = Database.forConfig("searcher.db")
  val repo = new SqlSuggestionsRepo()

  override def beforeAll(): Unit = {
    Await.ready(
      db.run((suggestions.schema ++ arguments.schema).createIfNotExists),
      Timeout
    )
  }

  override def afterAll(): Unit = {
    db.close()
  }

  "SuggestionsDBIO" should {

    "select suggestion by id" in {
      val action =
        for {
          id  <- db.run(repo.insert(suggestion.atom))
          res <- db.run(repo.select(id))
        } yield res

      Await.result(action, Timeout) shouldEqual Some(suggestion.atom)
    }

    "find suggestion by returnType" taggedAs Retry() in {
      val action =
        for {
          _   <- db.run(repo.insert(suggestion.local))
          _   <- db.run(repo.insert(suggestion.method))
          _   <- db.run(repo.insert(suggestion.function))
          res <- db.run(repo.findBy("MyType"))
        } yield res

      Await.result(action, Timeout) should contain theSameElementsAs Seq(
        suggestion.local,
        suggestion.function
      )
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
