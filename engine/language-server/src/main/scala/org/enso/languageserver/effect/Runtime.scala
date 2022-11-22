package org.enso.languageserver.effect

import scala.concurrent.ExecutionContext

object Runtime {

  /** Create ZIO [[zio.Runtime]] from execution context.
    *
    * @param ec the execution context to use in the runtime
    * @return a new instance of [[zio.Runtime]]
    */
  def fromExecutionContext(ec: ExecutionContext): zio.Runtime[zio.ZEnv] =
    zio.Runtime.unsafeFromLayer(
      zio.ZEnv.live,
      zio.internal.Platform.fromExecutionContext(ec)
    )
}
