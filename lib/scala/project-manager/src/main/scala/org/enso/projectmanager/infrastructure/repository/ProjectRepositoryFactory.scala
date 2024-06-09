package org.enso.projectmanager.infrastructure.repository
import java.io.File

trait ProjectRepositoryFactory[F[+_, +_]] {

  def getProjectRepository(
    projectsDirectory: Option[File]
  ): ProjectRepository[F]
}
