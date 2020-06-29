package org.enso.interpreter.test

import java.io.{
  ByteArrayOutputStream,
  PipedInputStream,
  PipedOutputStream,
  PrintStream
}
import java.util.UUID

import com.oracle.truffle.api.instrumentation.EventBinding
import org.enso.interpreter.test.CodeIdsTestInstrument.IdEventListener
import org.enso.interpreter.test.CodeLocationsTestInstrument.LocationsEventListener
import org.enso.polyglot.debugger.{
  DebugServerInfo,
  DebuggerSessionManagerEndpoint,
  ReplExecutor,
  SessionManager
}
import org.enso.polyglot.{
  Function,
  LanguageInfo,
  PolyglotContext,
  RuntimeOptions
}
import org.graalvm.polyglot.{Context, Value}
import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
case class LocationsInstrumenter(instrument: CodeLocationsTestInstrument) {
  var bindings: List[EventBinding[LocationsEventListener]] = List()

  def assertNodeExists(start: Int, length: Int, kind: Class[_]): Unit =
    bindings ::= instrument.bindTo(start, length, kind)

  def verifyResults(): Unit = {
    bindings.foreach { binding =>
      val listener = binding.getElement
      if (!listener.isSuccessful) {
        Assertions.fail(
          s"Node of type ${listener.getType.getSimpleName} at position ${listener.getStart} with length ${listener.getLength} was not found."
        )
      }
    }
  }

  def close(): Unit = {
    bindings.foreach(_.dispose)
  }
}

case class IdsInstrumenter(instrument: CodeIdsTestInstrument) {
  var bindings: List[EventBinding[IdEventListener]] = List()

  def assertNodeExists(id: UUID, result: String): Unit =
    bindings ::= instrument.bindTo(id, result)

  def assertNodeExistsTail(id: UUID): Unit =
    bindings ::= instrument.bindToTailCall(id)

  def verifyResults(): Unit = {
    bindings.foreach { binding =>
      val listener = binding.getElement
      if (!listener.isSuccessful) {
        Assertions.fail(
          s"Node with id ${listener.getId} does not exist or did not return the correct value."
        )
      }
    }
  }

  def close(): Unit = {
    bindings.foreach(_.dispose)
  }
}

class ReplaceableSessionManager extends SessionManager {
  var currentSessionManager: SessionManager = _
  def setSessionManager(manager: SessionManager): Unit =
    currentSessionManager = manager

  override def startSession(executor: ReplExecutor): Nothing =
    currentSessionManager.startSession(executor)
}

class InterpreterContext(
  contextModifiers: Context#Builder => Context#Builder = bldr => bldr
) {
  val output         = new ByteArrayOutputStream()
  val err            = new ByteArrayOutputStream()
  val inOut          = new PipedOutputStream()
  val inOutPrinter   = new PrintStream(inOut, true)
  val in             = new PipedInputStream(inOut)
  val sessionManager = new ReplaceableSessionManager

  val ctx = contextModifiers(
    Context
      .newBuilder(LanguageInfo.ID)
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .out(output)
      .err(err)
      .logHandler(System.err)
      .in(in)
      .serverTransport { (uri, peer) =>
        if (uri.toString == DebugServerInfo.URI) {
          new DebuggerSessionManagerEndpoint(sessionManager, peer)
        } else null
      }
  ).build()
  lazy val executionContext = new PolyglotContext(ctx)
}

trait InterpreterRunner {
  implicit class RichValue(value: Value) {
    def call(l: Long*): Value =
      InterpreterException.rethrowPolyglot(
        value.execute(l.map(_.asInstanceOf[AnyRef]): _*)
      )
  }

  def withLocationsInstrumenter(
    test: LocationsInstrumenter => Unit
  )(implicit interpreterContext: InterpreterContext): Unit = {
    val instrument = interpreterContext.ctx.getEngine.getInstruments
      .get(CodeLocationsTestInstrument.INSTRUMENT_ID)
      .lookup(classOf[CodeLocationsTestInstrument])
    val instrumenter = LocationsInstrumenter(instrument)
    test(instrumenter)
    instrumenter.verifyResults()
    instrumenter.close()
  }

  def withIdsInstrumenter(
    test: IdsInstrumenter => Unit
  )(implicit interpreterContext: InterpreterContext): Unit = {
    val instrument = interpreterContext.ctx.getEngine.getInstruments
      .get(CodeIdsTestInstrument.INSTRUMENT_ID)
      .lookup(classOf[CodeIdsTestInstrument])
    val instrumenter = IdsInstrumenter(instrument)
    test(instrumenter)
    instrumenter.verifyResults()
    instrumenter.close()
  }

  case class MainMethod(mainConstructor: Value, mainFunction: Function) {
    def execute(args: AnyRef*): Value =
      InterpreterException.rethrowPolyglot(
        mainFunction.execute(mainConstructor +: args: _*)
      )
  }

  def getMain(
    code: String
  )(implicit interpreterContext: InterpreterContext): MainMethod = {
    interpreterContext.output.reset()
    val module = InterpreterException.rethrowPolyglot(
      interpreterContext.executionContext.evalModule(code, "Test")
    )
    val assocCons    = module.getAssociatedConstructor
    val mainFunction = module.getMethod(assocCons, "main")
    MainMethod(assocCons, mainFunction)
  }

  def eval(
    code: String
  )(implicit interpreterContext: InterpreterContext): Value = {
    InterpreterException.rethrowPolyglot {
      val main = getMain(code)
      main.mainFunction.execute(main.mainConstructor)
    }
  }

  def consumeErr(implicit
    interpreterContext: InterpreterContext
  ): List[String] = {
    val result = interpreterContext.err.toString
    interpreterContext.err.reset()
    result.linesIterator.toList
  }

  def consumeOut(implicit
    interpreterContext: InterpreterContext
  ): List[String] = {
    val result = interpreterContext.output.toString
    interpreterContext.output.reset()
    result.linesIterator.toList
  }

  def feedInput(
    string: String
  )(implicit interpreterContext: InterpreterContext): Unit = {
    interpreterContext.inOutPrinter.println(string)
  }

  def setSessionManager(
    manager: SessionManager
  )(implicit interpreterContext: InterpreterContext): Unit =
    interpreterContext.sessionManager.setSessionManager(manager)

  // For Enso raw text blocks inside scala multiline strings
  val rawTQ = "\"\"\""
}

trait DefaultInterpreterRunner extends InterpreterRunner {
  implicit val interpreterContext: InterpreterContext = new InterpreterContext()
}

trait InterpreterBehavior {
  def subject: String

  def specify(implicit interpreterContext: InterpreterContext): Unit

  def contextModifiers: Context#Builder => Context#Builder = bldr => bldr
}

trait InterpreterTest
    extends AnyWordSpec
    with InterpreterBehavior
    with Matchers
    with ValueEquality
    with InterpreterRunner {

  subject when {
    "Context is Cached" should {
      behave like specify(new InterpreterContext(contextModifiers))
    }
  }

  subject when {
    "Context is Uncached" should {
      behave like specify(
        new InterpreterContext(
          contextModifiers
            .andThen(_.option(RuntimeOptions.DISABLE_INLINE_CACHES, "true"))
        )
      )
    }
  }
}
