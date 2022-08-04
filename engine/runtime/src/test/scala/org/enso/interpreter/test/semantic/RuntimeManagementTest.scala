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
        """import Standard.Base.Runtime.Thread
          |from Standard.Base.IO import all
          |import Standard.Base.Nothing
          |
          |foo x =
          |    if x == 0 then IO.println "Start." else Nothing
          |    @Tail_Call foo x+1
          |
          |main =
          |    Thread.with_interrupt_handler (foo 0) (IO.println "Interrupted.")
          |""".stripMargin

      val main = getMain(code)

      val runnable: Runnable = { () =>
        val p = langCtx.getThreadManager.enter()
        try {
          Try(main.execute())
        } finally {
          langCtx.getThreadManager.leave(p)
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
        langCtx.getThreadManager.interruptThreads()
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
          |from Standard.Base.Runtime.Resource import Managed_Resource
          |from Standard.Base.IO import all
          |
          |type Mock_File
          |    Mk_Mock_File i
          |
          |free_resource r = IO.println ("Freeing: " + r.to_text)
          |
          |create_resource i =
          |    c = Mk_Mock_File i
          |    r = Managed_Resource.register c free_resource
          |    r . with f-> IO.println ("Accessing: " + f.to_text)
          |
          |main =
          |    create_resource 0
          |    create_resource 1
          |    create_resource 2
          |    create_resource 3
          |    create_resource 4
          |""".stripMargin
      eval(code)
      var totalOut: List[String] = Nil

      forceGC()

      totalOut = consumeOut
      while (totalOut.length < 10) {
        Thread.sleep(100)
        totalOut ++= consumeOut
      }

      def mkAccessStr(i: Int): String = s"Accessing: (Mk_Mock_File $i)"
      def mkFreeStr(i: Int): String   = s"Freeing: (Mk_Mock_File $i)"
      def all                         = 0.to(4).map(mkAccessStr) ++ 0.to(4).map(mkFreeStr)
      totalOut should contain theSameElementsAs all
    }

    "Automatically free managed resources amongst manual closure of other managed resources" in {
      val code =
        """
          |from Standard.Base.Runtime.Resource import Managed_Resource
          |from Standard.Base.IO import all
          |import Standard.Base.Nothing
          |
          |type Mock_File
          |    Mk_Mock_File i
          |
          |free_resource r = IO.println ("Freeing: " + r.to_text)
          |
          |create_resource i =
          |    c = Mk_Mock_File i
          |    r = Managed_Resource.register c free_resource
          |    r . with f-> IO.println ("Accessing: " + f.to_text)
          |    if i % 2 == 0 then r.finalize else Nothing
          |
          |main =
          |    create_resource 0
          |    create_resource 1
          |    create_resource 2
          |    create_resource 3
          |    create_resource 4
          |""".stripMargin
      eval(code)
      var totalOut: List[String] = Nil

      forceGC()

      totalOut = consumeOut
      while (totalOut.length < 10) {
        Thread.sleep(100)
        totalOut ++= consumeOut
      }

      def mkAccessStr(i: Int): String = s"Accessing: (Mk_Mock_File $i)"
      def mkFreeStr(i: Int): String   = s"Freeing: (Mk_Mock_File $i)"
      def all                         = 0.to(4).map(mkAccessStr) ++ 0.to(4).map(mkFreeStr)
      totalOut should contain theSameElementsAs all
    }

    "Automatically free managed resources amongst manual takeover of other managed resources" in {
      val code =
        """
          |from Standard.Base.Runtime.Resource import Managed_Resource
          |from Standard.Base.IO import all
          |import Standard.Base.Nothing
          |
          |type Mock_File
          |    Mk_Mock_File i
          |
          |free_resource r = IO.println ("Freeing: " + r.to_text)
          |
          |create_resource i =
          |    c = Mk_Mock_File i
          |    r = Managed_Resource.register c free_resource
          |    r . with f-> IO.println ("Accessing: " + f.to_text)
          |    if i % 2 == 0 then r.take else Nothing
          |
          |main =
          |    create_resource 0
          |    create_resource 1
          |    create_resource 2
          |    create_resource 3
          |    create_resource 4
          |""".stripMargin
      eval(code)
      var totalOut: List[String] = Nil

      forceGC()

      totalOut = consumeOut
      while (totalOut.length < 7) {
        Thread.sleep(100)
        totalOut ++= consumeOut
      }

      def mkAccessStr(i: Int): String = s"Accessing: (Mk_Mock_File $i)"
      def mkFreeStr(i: Int): String   = s"Freeing: (Mk_Mock_File $i)"
      def all                         = 0.to(4).map(mkAccessStr) ++ List(1, 3).map(mkFreeStr)
      totalOut should contain theSameElementsAs all
    }
  }
}
