package org.enso.projectmanager.boot

import com.typesafe.scalalogging.LazyLogging
import zio.{Cause, Supervisor}
import zio.internal.stacktracer.Tracer
import zio.internal.stacktracer.impl.AkkaLineNumbersTracer
import zio.internal.tracing.TracingConfig
import zio.internal.{Executor, Platform, Tracing}

import scala.concurrent.ExecutionContext

/** An environment needed to execute ZIO actions.
  *
  * @param computeExecutionContext compute thread pool
  */
class ZioPlatform(computeExecutionContext: ExecutionContext)
    extends Platform
    with LazyLogging {

  override def executor: Executor =
    Executor.fromExecutionContext(2048)(computeExecutionContext)

  override val tracing = Tracing(
    Tracer.globallyCached(new AkkaLineNumbersTracer),
    TracingConfig.enabled
  )

  override def fatal(t: Throwable): Boolean =
    t.isInstanceOf[VirtualMachineError]

  override def reportFatal(t: Throwable): Nothing = {
    t.printStackTrace()
    try {
      System.exit(-1)
      throw t
    } catch { case _: Throwable => throw t }
  }

  override def reportFailure(cause: Cause[Any]): Unit =
    if (cause.died)
      logger.error(cause.prettyPrint)

  override def supervisor: Supervisor[Any] =
    Supervisor.none
}
