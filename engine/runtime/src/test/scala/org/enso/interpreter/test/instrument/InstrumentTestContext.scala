package org.enso.interpreter.test.instrument

import org.enso.polyglot.runtime.Runtime.Api

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

class InstrumentTestContext {
  protected val messageQueue: LinkedBlockingQueue[Api.Response] =
    new LinkedBlockingQueue()

  def receiveNone: Option[Api.Response] = {
    Option(messageQueue.poll())
  }

  def receiveOne(timeout: Long=10): Option[Api.Response] = {
    Option(messageQueue.poll(timeout, TimeUnit.SECONDS))
  }

  def receiveN(n: Int, timeout: Long=10): List[Api.Response] = {
    Iterator.continually(receiveOne(timeout)).take(n).flatten.toList
  }

  def receiveNIgnoreStdLib(n: Int): List[Api.Response] = {
    receiveN(n+1, 50).filter(excludeLibraryLoadingPayload)
  }

  private def excludeLibraryLoadingPayload(response: Api.Response): Boolean = response match {
    case Api.Response(None, Api.LibraryLoaded(_, _, _, _)) =>
      false
    case _ =>
      true
  }

}
