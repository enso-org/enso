package org.enso.interpreter.instrument.execution

import org.enso.interpreter.runtime.EnsoContext

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

/** A factory that creates new truffle threads on demand.
  *
  * @param context the language context
  * @param prefix the prefix for names of created threads
  */
class TruffleThreadFactory(context: EnsoContext, prefix: String)
    extends ThreadFactory {

  private val counter = new AtomicInteger(0)

  /** @inheritdoc */
  override def newThread(r: Runnable): Thread = {
    val thread = context.createThread(r)
    thread.setName(s"$prefix-${counter.incrementAndGet()}")

    thread
  }

}
