package org.enso.polyglot.debugger

import org.enso.polyglot.debugger.protocol.{
  ExceptionRepresentation,
  ObjectRepresentation
}

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SerializationTest extends AnyWordSpec with Matchers with EitherValue {

  "EvaluationRequest" should {
    "preserve all information when being serialized and deserialized" in {
      val expression = "2 + 2"
      val bytes      = Debugger.createEvaluationRequest(expression)
      val request    = Debugger.deserializeRequest(bytes).rightValue

      request shouldEqual EvaluationRequest(expression)
    }
  }

  "ListBindingsRequest" should {
    "preserve all information when being serialized and deserialized" in {
      val bytes   = Debugger.createListBindingsRequest()
      val request = Debugger.deserializeRequest(bytes).rightValue

      request shouldEqual ListBindingsRequest
    }
  }

  "SessionExitRequest" should {
    "preserve all information when being serialized and deserialized" in {
      val bytes   = Debugger.createSessionExitRequest()
      val request = Debugger.deserializeRequest(bytes).rightValue

      request shouldEqual SessionExitRequest
    }
  }

  private def objectRepresentationIsConsistent(
    obj: Object,
    repr: ObjectRepresentation
  ): Boolean =
    obj.toString == repr.representation()

  "EvaluationSuccess" should {
    "preserve all information when being serialized and deserialized" in {
      val result   = ("String", 42)
      val bytes    = Debugger.createEvaluationSuccess(result)
      val response = Debugger.deserializeResponse(bytes).rightValue

      response should matchPattern { case EvaluationSuccess(_) => }
      val EvaluationSuccess(repr) = response
      assert(objectRepresentationIsConsistent(result, repr))
    }
  }

  private def assertStackTraceElementRepresentationIsConsistent(
    stackTraceElement: StackTraceElement,
    repr: protocol.StackTraceElement
  ): Unit = {
    repr.declaringClass() shouldEqual stackTraceElement.getClassName
    repr.fileName() shouldEqual stackTraceElement.getFileName
    repr.lineNumber() shouldEqual stackTraceElement.getLineNumber
    repr.methodName() shouldEqual stackTraceElement.getMethodName
  }

  private def getExceptionMessageFailsafe(ex: Throwable): String =
    Option(ex.getMessage).getOrElse(ex.toString)

  private def assertExceptionRepresentationIsConsistent(
    ex: Throwable,
    repr: ExceptionRepresentation
  ): Unit = {

    if (ex.getCause == null) {
      assert(repr.cause() == null, "cause in representation should be null too")
    } else {
      assertExceptionRepresentationIsConsistent(ex.getCause, repr.cause())
    }

    repr.message() shouldEqual getExceptionMessageFailsafe(ex)
    val trace = ex.getStackTrace

    repr.stackTraceLength() shouldEqual trace.length
    trace.zipWithIndex.foreach({
      case (traceElement: StackTraceElement, j: Int) =>
        assertStackTraceElementRepresentationIsConsistent(
          traceElement,
          repr.stackTrace(j)
        )
    })
  }

  "EvaluationFailure" should {
    "preserve all information when being serialized and deserialized" in {
      val exception = new RuntimeException("Test")
      val bytes     = Debugger.createEvaluationFailure(exception)
      val response  = Debugger.deserializeResponse(bytes).rightValue

      response should matchPattern { case EvaluationFailure(_) => }
      val EvaluationFailure(repr) = response
      assertExceptionRepresentationIsConsistent(exception, repr)
    }

    "handle null in exception message" in {
      val exception = new RuntimeException(null, null)
      val bytes     = Debugger.createEvaluationFailure(exception)
      val response  = Debugger.deserializeResponse(bytes).rightValue

      response should matchPattern { case EvaluationFailure(_) => }
      val EvaluationFailure(repr) = response
      assertExceptionRepresentationIsConsistent(exception, repr)
    }

    "handle nested exceptions" in {
      val exception = new RuntimeException("test", new RuntimeException)
      val bytes     = Debugger.createEvaluationFailure(exception)
      val response  = Debugger.deserializeResponse(bytes).rightValue

      response should matchPattern { case EvaluationFailure(_) => }
      val EvaluationFailure(repr) = response
      assertExceptionRepresentationIsConsistent(exception, repr)
    }
  }

  "ListBindingsResult" should {
    "preserve all information when being serialized and deserialized" in {
      val bindings: Map[String, Object] = Map(
        "a" -> "Test",
        "b" -> int2Integer(42)
      )
      val bytes    = Debugger.createListBindingsResult(bindings)
      val response = Debugger.deserializeResponse(bytes).rightValue

      response should matchPattern { case ListBindingsResult(_) => }
      val ListBindingsResult(bindingsRepr) = response

      bindingsRepr.size shouldEqual bindings.size
      assert(
        bindings forall {
          case (name: String, value: Object) =>
            objectRepresentationIsConsistent(value, bindingsRepr(name))
        },
        "bindings representations should be consistent"
      )
    }
  }

  "SessionExitNotification" should {
    "preserve all information when being serialized and deserialized" in {
      val bytes    = Debugger.createSessionStartNotification()
      val response = Debugger.deserializeResponse(bytes).rightValue

      response shouldEqual SessionStartNotification
    }
  }
}
