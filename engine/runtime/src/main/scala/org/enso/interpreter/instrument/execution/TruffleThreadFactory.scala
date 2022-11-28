package org.enso.interpreter.instrument.execution

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

import org.enso.interpreter.runtime.Context

/** A factory that creates new truffle threads on demand.
  *
  * @param context the language context
  * @param prefix the prefix for names of created threads
  */
class TruffleThreadFactory(context: Context, prefix: String)
    extends ThreadFactory {

  private val counter = new AtomicInteger(0)

  /** @inheritdoc */
  override def newThread(r: Runnable): Thread = {
    val thread = context.createThread(r)
    thread.setName(s"$prefix-${counter.incrementAndGet()}")

    thread
  }

}
