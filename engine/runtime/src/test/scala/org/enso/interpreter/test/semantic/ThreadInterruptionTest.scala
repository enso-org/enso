package org.enso.interpreter.test.semantic

import org.enso.interpreter.runtime.Context
import org.enso.interpreter.test.InterpreterTest
import org.enso.polyglot.{LanguageInfo, MethodNames}
import org.graalvm.polyglot.Value

import scala.util.{Failure, Try}

class ThreadInterruptionTest extends InterpreterTest {
  "Execution of Enso code" should "be interruptible through Thread#interrupt()" in {
    val langCtx = ctx
      .getBindings(LanguageInfo.ID)
      .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
      .asHostObject[Context]()

    val code =
      """
        |foo = here.foo
        |
        |main =
        |    Thread.with_interrupt_handler here.foo (IO.println "Interrupted.")
        |""".stripMargin

    val main = getMain(code)

    val runnable: Runnable = { () =>
      langCtx.getThreadManager.enter()
      try {
        Try(main.execute())
      } finally {
        langCtx.getThreadManager.leave()
      }
    }

    def runTest(n: Int = 5): Unit = {
      val expectedOut = List.fill(n)("Interrupted.")
      val threads = 0.until(n).map(_ => new Thread(runnable))
      threads.foreach(_.start())
      Thread.sleep(200)
      threads.foreach(_.interrupt())
      langCtx.getThreadManager.checkInterrupts()
      threads.foreach(_.join())
      consumeOut shouldEqual expectedOut
      threads.forall(!_.isAlive) shouldBe true
    }

    runTest()
    runTest()
  }
}
