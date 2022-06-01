package org.enso.interpreter.test.instrument

import org.enso.polyglot.runtime.Runtime.Api

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

class InstrumentTestContext {
  protected val messageQueue: LinkedBlockingQueue[Api.Response] =
    new LinkedBlockingQueue()

  def receiveNone: Option[Api.Response] = {
    Option(messageQueue.poll())
  }

  def receive: Option[Api.Response] = {
    Option(messageQueue.poll(10, TimeUnit.SECONDS))
  }

  def receiveWithTimeout(timeoutSeconds: Long): Option[Api.Response] = {
    Option(messageQueue.poll(timeoutSeconds, TimeUnit.SECONDS))
  }

  def receiveN(n: Int, timeoutSeconds: Long = 10): List[Api.Response] = {
    Iterator
      .continually(receiveWithTimeout(timeoutSeconds))
      .take(n)
      .flatten
      .toList
  }

  def receiveNIgnoreStdLib(
    n: Int,
    timeoutSeconds: Long = 60
  ): List[Api.Response] = {
    var count: Int                     = n
    var lastSeen: Option[Api.Response] = None
    Iterator
      .continually(receiveWithTimeout(timeoutSeconds))
      .takeWhile {
        case Some(Api.Response(None, Api.LibraryLoaded(_, _, _, _))) =>
          count > 0
        case msg @ Some(_) =>
          count -= 1
          lastSeen = msg
          count > 0
        case None =>
          false
      }
      .flatten
      .filter(excludeLibraryLoadingPayload)
      .toList ++ lastSeen
  }

  private def excludeLibraryLoadingPayload(response: Api.Response): Boolean =
    response match {
      case Api.Response(None, Api.LibraryLoaded(_, _, _, _)) =>
        false
      case _ =>
        true
    }

}

object InstrumentTestContext {
  val DISABLE_IR_CACHE =
    Option(System.getenv("ENSO_TEST_DISABLE_IR_CACHE")).getOrElse("true")
}
