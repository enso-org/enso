package org.enso.compiler.test

import org.enso.interpreter.test.InterpreterException

class CachingTest extends ModifiedTest {
  "Atoms and methods" should "be available for import" in {
    evalTestProjectIteration("Test_Caching_Invalidation", iteration = 1)
    val outLines = consumeOut
    outLines(0) shouldEqual "hmm..."

    evalTestProjectIteration("Test_Caching_Invalidation", iteration = 2)
    val outLines2 = consumeOut
    outLines2(0) shouldEqual "hmm..."

    the[InterpreterException] thrownBy (evalTestProjectIteration(
      "Test_Caching_Invalidation",
      iteration = 3
    )) should have message "Compilation aborted due to errors."
    val outLines3 = consumeOut
    outLines3(2) should endWith("The name `foo` could not be found.")
  }
}
