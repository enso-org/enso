package org.enso.interpreter.test.semantic

import org.enso.interpreter.runtime.Context
import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}
import org.enso.polyglot.{LanguageInfo, MethodNames}

import scala.ref.WeakReference
import scala.util.Try

class RuntimeManagementTest extends InterpreterTest {
  override def subject: String = "Enso Code Execution"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "Interrupt threads through Thread#interrupt()" in {
      val langCtx = interpreterContext.ctx
        .getBindings(LanguageInfo.ID)
        .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
        .asHostObject[Context]()

      val code =
        """from Builtins import all
          |
          |foo x =
          |    if x == 0 then IO.println "Start." else Nothing
          |    @Tail_Call here.foo x+1
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

    /** Don't use this in production code, ever.
      */
    def forceGC(): Unit = {
      var obj = new Object
      val ref = new WeakReference[Object](obj)
      obj = null
      while (ref.get.isDefined) {
        System.gc()
      }
    }

    "Automatically free managed resources" in {
      val code =
        """
          |from Builtins import all
          |
          |type Mock_File i
          |
          |free_resource r = IO.println ("Freeing: " + r.to_text)
          |
          |create_resource i =
          |    c = Mock_File i
          |    r = Managed_Resource.register c here.free_resource
          |    Managed_Resource.with r f-> IO.println ("Accessing: " + f.to_text)
          |
          |main =
          |    here.create_resource 0
          |    here.create_resource 1
          |    here.create_resource 2
          |    here.create_resource 3
          |    here.create_resource 4
          |""".stripMargin
      eval(code)
      var totalOut: List[String] = Nil

      forceGC()

      totalOut = consumeOut
      while (totalOut.length < 10) {
        Thread.sleep(100)
        totalOut ++= consumeOut
      }

      def mkAccessStr(i: Int): String = s"Accessing: (Mock_File $i)"
      def mkFreeStr(i: Int): String   = s"Freeing: (Mock_File $i)"
      def all                         = 0.to(4).map(mkAccessStr) ++ 0.to(4).map(mkFreeStr)
      totalOut should contain theSameElementsAs all
    }

    "Automatically free managed resources amongst manual closure of other managed resources" in {
      val code =
        """
          |from Builtins import all
          |
          |type Mock_File i
          |
          |free_resource r = IO.println ("Freeing: " + r.to_text)
          |
          |create_resource i =
          |    c = Mock_File i
          |    r = Managed_Resource.register c here.free_resource
          |    Managed_Resource.with r f-> IO.println ("Accessing: " + f.to_text)
          |    if i % 2 == 0 then Managed_Resource.finalize r else Nothing
          |
          |main =
          |    here.create_resource 0
          |    here.create_resource 1
          |    here.create_resource 2
          |    here.create_resource 3
          |    here.create_resource 4
          |""".stripMargin
      eval(code)
      var totalOut: List[String] = Nil

      forceGC()

      totalOut = consumeOut
      while (totalOut.length < 10) {
        Thread.sleep(100)
        totalOut ++= consumeOut
      }

      def mkAccessStr(i: Int): String = s"Accessing: (Mock_File $i)"
      def mkFreeStr(i: Int): String   = s"Freeing: (Mock_File $i)"
      def all                         = 0.to(4).map(mkAccessStr) ++ 0.to(4).map(mkFreeStr)
      totalOut should contain theSameElementsAs all
    }

    "Automatically free managed resources amongst manual takeover of other managed resources" in {
      val code =
        """
          |from Builtins import all
          |
          |type Mock_File i
          |
          |free_resource r = IO.println ("Freeing: " + r.to_text)
          |
          |create_resource i =
          |    c = Mock_File i
          |    r = Managed_Resource.register c here.free_resource
          |    Managed_Resource.with r f-> IO.println ("Accessing: " + f.to_text)
          |    if i % 2 == 0 then Managed_Resource.take r else Nothing
          |
          |main =
          |    here.create_resource 0
          |    here.create_resource 1
          |    here.create_resource 2
          |    here.create_resource 3
          |    here.create_resource 4
          |""".stripMargin
      eval(code)
      var totalOut: List[String] = Nil

      forceGC()

      totalOut = consumeOut
      while (totalOut.length < 7) {
        Thread.sleep(100)
        totalOut ++= consumeOut
      }

      def mkAccessStr(i: Int): String = s"Accessing: (Mock_File $i)"
      def mkFreeStr(i: Int): String   = s"Freeing: (Mock_File $i)"
      def all                         = 0.to(4).map(mkAccessStr) ++ List(1, 3).map(mkFreeStr)
      totalOut should contain theSameElementsAs all
    }
  }
}
