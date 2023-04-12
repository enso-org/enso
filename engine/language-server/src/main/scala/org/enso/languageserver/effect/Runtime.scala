package org.enso.languageserver.effect

import scala.concurrent.ExecutionContext

/** Object holding an instance of ZIO runtime capable of running effects. */
trait Runtime {

  /** @return the runtime instance */
  def instance: zio.Runtime[zio.ZAny]

  /** Initialize the runtime. */
  def init(): Unit
}

object Runtime {

  /** Create ZIO [[zio.Runtime]] from execution context.
    *
    * @param ec the execution context to use in the runtime
    * @return a new instance of [[zio.Runtime]]
    */
  def fromExecutionContext(
    ec: ExecutionContext
  ): zio.Runtime[zio.ZAny] =
    zio.Unsafe.unsafe { implicit unsafe =>
      zio.Runtime.unsafe.fromLayer(
        zio.Runtime.setExecutor(zio.Executor.fromExecutionContext(ec))
      )
    }
}

/** Runtime that executes effects in the provided execution context.
  *
  * @param ec the execution context to use in the runtime
  */
final class ExecutionContextRuntime(ec: ExecutionContext) extends Runtime {

  var instance: zio.Runtime[zio.ZAny] = _

  /** Initialize the runtime. */
  override def init(): Unit =
    synchronized {
      if (instance eq null) {
        instance = Runtime.fromExecutionContext(ec)
      }
    }
}
