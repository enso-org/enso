package org.enso.runner

import com.typesafe.scalalogging.Logger
import org.enso.cli.ProgressBar
import org.enso.cli.task.{ProgressReporter, TaskProgress}
import org.enso.libraryupload.{auth, LibraryUploader}

import java.nio.file.Path

object ProjectUploader {

  private lazy val logger = Logger[ProjectUploader.type]

  def uploadProject(
    projectRoot: Path,
    uploadUrl: String,
    authToken: Option[String],
    showProgress: Boolean
  ): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val progressReporter = new ProgressReporter {
      override def trackProgress(
        message: String,
        task: TaskProgress[_]
      ): Unit = {
        logger.info(message)
        if (showProgress) {
          ProgressBar.waitWithProgress(task)
        }
      }
    }

    val token = authToken match {
      case Some(value) => auth.SimpleHeaderToken(value)
      case None        => auth.NoAuthorization
    }
    LibraryUploader
      .uploadLibrary(
        projectRoot,
        uploadUrl,
        token,
        progressReporter
      )
      .get
  }
}
