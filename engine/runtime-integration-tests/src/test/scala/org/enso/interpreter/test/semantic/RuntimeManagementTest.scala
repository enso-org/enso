package org.enso.interpreter.test.semantic

import org.enso.interpreter.runtime.EnsoContext
import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}
import org.enso.common.{LanguageInfo, MethodNames}

import scala.ref.WeakReference
import org.graalvm.polyglot.Context
import org.enso.common.RuntimeOptions

class RuntimeManagementTest extends InterpreterTest {
  private def parallelism = 5

  override def subject: String = "Enso Code Execution"

  override def contextModifiers: Option[Context#Builder => Context#Builder] =
    Some(b => {
      b.allowCreateThread(true)
        .option(RuntimeOptions.GUEST_PARALLELISM, "" + parallelism)
    })

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "Interrupt threads through Thread#interrupt()" in {
      val langCtx = interpreterContext
        .ctx()
        .getBindings(LanguageInfo.ID)
        .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
        .asHostObject[EnsoContext]()

      val code =
        """import Standard.Base.Runtime.Thread
          |import Standard.Base.IO
          |import Standard.Base.Nothing
          |import Standard.Base.Data.Numbers.Number
          |
          |foo x =
          |    if x == 0 then IO.println "Start." else Nothing
          |    @Tail_Call foo x+1
          |
          |main =
          |    Thread.with_interrupt_handler (foo 0) (IO.println "Interrupted.")
          |""".stripMargin

      val main = getMain(code)

      def runMain(): java.util.concurrent.Future[org.graalvm.polyglot.Value] =
        langCtx.getThreadManager.submit(() => {
          main.execute()
        })

      def runTest(): Unit = {
        val futures       = 0.until(parallelism).map(_ => runMain())
        var reportedCount = 0
        while (reportedCount < parallelism) {
          Thread.sleep(100)
          reportedCount += consumeOut.length
        }
        val expectedOut = List.fill(parallelism)("Interrupted.")
        langCtx.getThreadManager.interruptThreads()
        futures.foreach(f => {
          try {
            val v = f.get()
            fail("Unexpected value: " + v)
          } catch {
            case ex1: java.util.concurrent.ExecutionException => {
              ex1.getMessage() shouldEqual "org.enso.interpreter.test.InterpreterException: org.enso.interpreter.runtime.control.ThreadInterruptedException"
            }
          }
        })
        consumeOut shouldEqual expectedOut
        futures.forall(_.isDone) shouldBe true
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

    def consumeWithGC(expect: Int): List[String] = {
      forceGC()
      var round                  = 0
      var totalOut: List[String] = Nil
      totalOut = consumeOut
      while (totalOut.length < expect && round < 500) {
        round = round + 1
        if (round % 10 == 0) {
          forceGC();
        }
        val res = eval("main a b = a + b").execute("Hello", "Enso")
        assertResult("HelloEnso")(res.asString)
        Thread.sleep(100)
        totalOut ++= consumeOut
      }
      totalOut
    }

    "Automatically free managed resources" in {
      val code =
        """
          |from Standard.Base.Runtime.Resource import Managed_Resource
          |import Standard.Base.IO
          |
          |type Mock_File
          |    Value i
          |
          |free_resource r = IO.println ("Freeing: " + r.to_text)
          |
          |create_resource i =
          |    c = Mock_File.Value i
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
      val totalOut = consumeWithGC(10)

      def mkAccessStr(i: Int): String = s"Accessing: (Mock_File.Value $i)"
      def mkFreeStr(i: Int): String   = s"Freeing: (Mock_File.Value $i)"
      def all                         = 0.to(4).map(mkAccessStr) ++ 0.to(4).map(mkFreeStr)
      totalOut should contain theSameElementsAs all
    }

    "Automatically free managed resources amongst manual closure of other managed resources" in {
      val code =
        """
          |from Standard.Base.Runtime.Resource import Managed_Resource
          |import Standard.Base.IO
          |import Standard.Base.Nothing
          |import Standard.Base.Data.Numbers.Number
          |
          |type Mock_File
          |    Value i
          |
          |free_resource r = IO.println ("Freeing: " + r.to_text)
          |
          |create_resource i =
          |    c = Mock_File.Value i
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
      val totalOut = consumeWithGC(10)

      def mkAccessStr(i: Int): String = s"Accessing: (Mock_File.Value $i)"
      def mkFreeStr(i: Int): String   = s"Freeing: (Mock_File.Value $i)"
      def all                         = 0.to(4).map(mkAccessStr) ++ 0.to(4).map(mkFreeStr)
      totalOut should contain theSameElementsAs all
    }

    "Automatically free managed resources amongst manual takeover of other managed resources" in {
      val code =
        """
          |from Standard.Base.Runtime.Resource import Managed_Resource
          |import Standard.Base.IO
          |import Standard.Base.Nothing
          |import Standard.Base.Data.Numbers.Number
          |
          |type Mock_File
          |    Value i
          |
          |free_resource r = IO.println ("Freeing: " + r.to_text)
          |
          |create_resource i =
          |    c = Mock_File.Value i
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
      val totalOut = consumeWithGC(7)

      def mkAccessStr(i: Int): String = s"Accessing: (Mock_File.Value $i)"
      def mkFreeStr(i: Int): String   = s"Freeing: (Mock_File.Value $i)"
      def all                         = 0.to(4).map(mkAccessStr) ++ List(1, 3).map(mkFreeStr)
      totalOut should contain theSameElementsAs all
    }

    "Allow for multithreaded polyglot class loading" in {
      val langCtx = interpreterContext
        .ctx()
        .getBindings(LanguageInfo.ID)
        .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
        .asHostObject[EnsoContext]()

      val code =
        """import Standard.Base.Data.Numbers
          |polyglot java import org.enso.example.TestClass
          |main =
          |    instance = TestClass.new (x -> x * 2)
          |    instance.callFunctionAndIncrement 10
          |""".stripMargin

      val main = getMain(code)

      def runMain()
        : java.util.concurrent.CompletableFuture[org.graalvm.polyglot.Value] =
        langCtx.getThreadManager.submit(() => {
          main.execute()
        })

      def runTest(): Unit = {
        val futures = 0.until(parallelism).map(_ => runMain())
        val combinedFuture = java.util.concurrent.CompletableFuture
          .allOf(futures: _*)
          .thenApply(_ => {
            futures
              .map(_.get(10, java.util.concurrent.TimeUnit.SECONDS).asInt())
          })
        val result =
          combinedFuture.get(20, java.util.concurrent.TimeUnit.SECONDS)
        result should equal(List(21, 21, 21, 21, 21))
        futures.forall(_.isDone) shouldBe true
      }

      runTest()
    }
  }
}
