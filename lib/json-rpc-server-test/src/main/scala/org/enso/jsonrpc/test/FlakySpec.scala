package org.enso.jsonrpc.test

import org.scalatest._

/**
  * Trait is used to mark the tests in the suite as _flaky_ and make them
  * pass. It changes the behavior for failed tests to return 'pending' instead
  * of failing the suite.
  */
trait FlakySpec extends TestSuite {

  /** Tags test as _flaky_. */
  object Flaky extends Tag("flaky")

  override def withFixture(test: NoArgTest): Outcome =
    super.withFixture(test) match {
      case Failed(_) | Canceled(_) if test.tags.contains(Flaky.name) =>
        Pending
      case outcome =>
        outcome
    }
}
