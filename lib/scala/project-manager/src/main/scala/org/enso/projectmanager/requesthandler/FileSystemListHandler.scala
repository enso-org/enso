package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.FileSystemManagementApi.FileSystemList
import org.enso.projectmanager.service.filesystem.{
  FileSystemServiceApi,
  FileSystemServiceFailure
}

import scala.concurrent.duration.FiniteDuration

/** A request handler for `filesystem/list` commands.
  *
  * @param fileSystemService a filesystem service
  * @param requestTimeout a request timeout
  * @param timeoutRetries a number of timeouts to wait until a failure is reported
  */
class FileSystemListHandler[F[+_, +_]: Exec: CovariantFlatMap](
  fileSystemService: FileSystemServiceApi[F],
  requestTimeout: FiniteDuration,
  timeoutRetries: Int
) extends RequestHandler[
      F,
      FileSystemServiceFailure,
      FileSystemList.type,
      FileSystemList.Params,
      FileSystemList.Result
    ](
      FileSystemList,
      Some(requestTimeout),
      timeoutRetries
    ) {

  override def handleRequest = { params =>
    for {
      projects <- fileSystemService.list(params.path)
    } yield FileSystemList.Result(projects)
  }

}

object FileSystemListHandler {

  /** Creates a configuration object used to create a [[FileSystemListHandler]].
    *
    * @param service a filesystem service
    * @param requestTimeout a request timeout
    * @param timeoutRetries a number of timeouts to wait until a failure is reported
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    service: FileSystemServiceApi[F],
    requestTimeout: FiniteDuration,
    timeoutRetries: Int
  ): Props =
    Props(
      new FileSystemListHandler(service, requestTimeout, timeoutRetries)
    )

}
