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

  private def stackTraceElementRepresentationIsConsistent(
    stackTraceElement: StackTraceElement,
    repr: protocol.StackTraceElement
  ): Boolean = {
    repr.declaringClass() == stackTraceElement.getClassName &&
    repr.fileName() == stackTraceElement.getFileName &&
    repr.lineNumber() == stackTraceElement.getLineNumber &&
    repr.methodName() == stackTraceElement.getMethodName
  }

  private def exceptionRepresentationIsConsistent(
    ex: Throwable,
    repr: ExceptionRepresentation
  ): Boolean = {
    val causeIsConsistent =
      (repr.cause() == null && ex.getCause == null) || (exceptionRepresentationIsConsistent(
        ex.getCause,
        repr.cause()
      ))
    val messageIsConsistent = repr.message() == ex.getMessage
    val trace               = ex.getStackTrace
    val traceIsConsistent =
      (trace.length == repr.stackTraceLength()) &&
      trace.zipWithIndex.forall({
        case (traceElement: StackTraceElement, j: Int) =>
          stackTraceElementRepresentationIsConsistent(
            traceElement,
            repr.stackTrace(j)
          )
      })

    causeIsConsistent && messageIsConsistent && traceIsConsistent
  }

  "EvaluationFailure" should {
    "preserve all information when being serialized and deserialized" in {
      val exception = new RuntimeException("Test")
      val bytes     = Debugger.createEvaluationFailure(exception)
      val response  = Debugger.deserializeResponse(bytes).rightValue

      response should matchPattern { case EvaluationFailure(_) => }
      val EvaluationFailure(repr) = response
      assert(
        exceptionRepresentationIsConsistent(exception, repr),
        "exception representation should be consistent"
      )
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
