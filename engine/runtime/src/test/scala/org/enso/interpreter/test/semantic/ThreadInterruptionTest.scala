package org.enso.interpreter.test.semantic

import org.enso.interpreter.runtime.Context
import org.enso.interpreter.test.{InterpreterTest, InterpreterContext}
import org.enso.polyglot.{LanguageInfo, MethodNames}

import scala.util.Try

class ThreadInterruptionTest extends InterpreterTest {
  override def subject: String = "Enso Code Execution"

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {

    "be interruptible through Thread#interrupt()" in {
      val langCtx = interpreterContext.ctx
        .getBindings(LanguageInfo.ID)
        .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
        .asHostObject[Context]()

      val code =
        """
          |foo x =
          |    if x == 0 then IO.println "Start." else Unit
          |    here.foo x+1
          |
          |main =
          |    Thread.with_interrupt_handler (here.foo 0) (IO.println "Interrupted.")
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
        val threads = 0.until(n).map(_ => new Thread(runnable))
        threads.foreach(_.start())
        var reportedCount = 0
        while (reportedCount < n) {
          Thread.sleep(100)
          reportedCount += consumeOut.length
        }
        val expectedOut = List.fill(n)("Interrupted.")
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
}
