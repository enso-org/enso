package src.main.scala.licenses.frontend

import java.nio.file.Path

import com.typesafe.sbt.license.{DepLicense, DepModuleInfo, LicenseReport}
import com.typesafe.sbt.license.SbtCompat.{
  IvyRetrieve,
  IvySbt,
  ResolveException
}
import jdk.jfr.internal.TypeLibrary
import org.apache.ivy.core.report.ResolveReport
import org.apache.ivy.core.resolve.IvyNode
import sbt.Compile
import sbt.internal.util.ManagedLogger
import sbt.io.IO
import sbt.librarymanagement.ConfigRef
import src.main.scala.licenses.{
  DependencyInformation,
  SBTDistributionComponent,
  SourceAccess
}

import scala.collection.JavaConverters._

object SbtLicenses {
  val relevantConfigurations = Seq(Compile)

  def analyze(
    components: Seq[SBTDistributionComponent],
    log: ManagedLogger
  ): (Seq[DependencyInformation], Seq[String]) = {
    val results = components.map { component =>
      val report  = resolveIvy(component.ivyModule, log)
      val ivyDeps = report.getDependencies.asScala.map(_.asInstanceOf[IvyNode])
      val sourceArtifacts = component.classifiedArtifactsReport
        .select((configRef: ConfigRef) =>
          relevantConfigurations.map(_.name).contains(configRef.name)
        )
        .map(_.toPath)
        .filter(_.getFileName.toString.endsWith("sources.jar"))
      val deps = for {
        dep <- component.licenseReport.licenses
        depNode =
          ivyDeps
            .find(ivyDep => safeModuleInfo(ivyDep) == Some(dep.module))
            .getOrElse(
              throw new RuntimeException(
                s"Could not find Ivy node for resolved module ${dep.module}."
              )
            )
      } yield {
        val sources = sourceArtifacts.filter(
          _.getFileName.toString.startsWith(dep.module.name)
        )
        Dependency(dep, depNode, sources)
      }
      (deps, sourceArtifacts)
    }

    val distinctDependencies =
      results.flatMap(_._1).groupBy(_.depLicense.module).map(_._2.head).toSeq
    val distinctSources = results.flatMap(_._2).distinct

    val wrappedDeps =
      for (dependency <- distinctDependencies)
        yield DependencyInformation(
          moduleInfo = dependency.depLicense.module,
          license    = dependency.depLicense.license,
          sources    = findSources(dependency),
          url        = tryFindingUrl(dependency)
        )
    val relevantDeps = wrappedDeps.filter(DependencyFilter.shouldKeep)

    val missingWarnings = for {
      dep <- relevantDeps
      if dep.sources.isEmpty
    } yield s"Could not find sources for ${dep.moduleInfo}"
    val unexpectedWarnings = for {
      source <- distinctSources
      if !distinctDependencies.exists(_.sourcesJARPath.contains(source))
    } yield s"Found a source $source that does not belong to any known dependencies, perhaps the algorithm needs updating?"

    (relevantDeps, missingWarnings ++ unexpectedWarnings)
  }

  private def resolveIvy(
    ivyModule: IvySbt#Module,
    log: ManagedLogger
  ): ResolveReport = {
    val (report, err) = LicenseReport.resolve(ivyModule, log)
    err.foreach(throw _)
    report
  }

  private def tryFindingUrl(dependency: Dependency): Option[String] =
    Option(dependency.ivyNode.getDescriptor).flatMap(descriptor =>
      Option(descriptor.getHomePage)
    )

  private def createSourceAccessFromJAR(jarPath: Path): SourceAccess =
    new SourceAccess {
      override def access[R](withSources: Path => R): R =
        IO.withTemporaryDirectory { root =>
          IO.unzip(jarPath.toFile, root)
          withSources(root.toPath)
        }
    }

  private def findSources(dependency: Dependency): Seq[SourceAccess] =
    dependency.sourcesJARPath.map(createSourceAccessFromJAR)

  case class Dependency(
    depLicense: DepLicense,
    ivyNode: IvyNode,
    sourcesJARPath: Seq[Path]
  )

  private def safeModuleInfo(dep: IvyNode): Option[DepModuleInfo] =
    for {
      moduleId       <- Option(dep.getModuleId)
      moduleRevision <- Option(dep.getModuleRevision)
      revisionId     <- Option(moduleRevision.getId)
    } yield DepModuleInfo(
      moduleId.getOrganisation,
      moduleId.getName,
      revisionId.getRevision
    )
}
