package org.enso.languageserver.requesthandler.workspace

import akka.actor.{Actor, ActorLogging, Props}
import buildinfo.Info
import org.enso.jsonrpc.{Request, ResponseError, ResponseResult}
import org.enso.languageserver.data.Config
import org.enso.languageserver.filemanager.FileManagerApi
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.workspace.WorkspaceApi.ProjectInfo
import org.enso.logger.masking.MaskedPath
import org.enso.pkg.{Config => PkgConfig}

import java.io.{File, FileInputStream}
import java.nio.charset.StandardCharsets

/** A request handler for `workspace/openFile` commands.
  */
class ProjectInfoHandler(languageServerConfig: Config)
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def receive: Receive = { case Request(ProjectInfo, id, _) =>
    val projectRoot = languageServerConfig.directories.root.toPath.toFile
    val configFile  = new File(projectRoot, Config.ensoPackageConfigName)

    if (configFile.exists()) {
      val projectConfig = PkgConfig.fromYaml(
        new String(
          new FileInputStream(configFile).readAllBytes(),
          StandardCharsets.UTF_8
        )
      )
      if (projectConfig.isSuccess) {
        val projectInfo = ProjectInfo.Result(
          projectName   = projectConfig.get.name,
          engineVersion = Info.ensoVersion,
          graalVersion  = Info.graalVersion
        )
        sender() ! ResponseResult(
          ProjectInfo,
          id,
          projectInfo
        )
      } else {
        log.error(
          "Could not decode the package configuration at [{}].",
          MaskedPath(configFile.toPath)
        )
        sender() ! ResponseError(Some(id), FileManagerApi.CannotDecodeError)
      }
    } else {
      log.error(
        "Could not find the package configuration in the project at [{}].",
        MaskedPath(projectRoot.toPath)
      )
      sender() ! ResponseError(Some(id), FileManagerApi.FileNotFoundError)
    }
  }
}
object ProjectInfoHandler {

  /** Creates a configuration object used to create a [[ProjectInfoHandler]].
    *
    * @return a configuration object
    */
  def props(languageServerConfig: Config): Props = Props(
    new ProjectInfoHandler(languageServerConfig)
  )
}
