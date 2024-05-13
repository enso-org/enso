package src.main.scala.licenses.frontend

import java.nio.file.Path
import sbtlicensereport.license.{DepLicense, DepModuleInfo}
import org.apache.ivy.core.resolve.IvyNode
import sbt.Compile
import sbt.internal.util.ManagedLogger
import sbt.io.IO
import sbt.librarymanagement.ConfigRef
import src.main.scala.licenses.report.Diagnostic
import src.main.scala.licenses.{
  DependencyInformation,
  SBTDistributionComponent,
  SourceAccess
}

import scala.collection.JavaConverters._

/** Defines the algorithm for discovering dependency metadata.
  */
object SbtLicenses {

  /** Defines configurations that are deemed relevant for dependency discovery.
    *
    * Currently we only analyse Compile dependencies as these are the ones that
    * get packaged.
    *
    * Provided dependencies are assumed to be already present in the used
    * runtime, so we do not distribute them. One exception is the launcher which
    * does distribute the provided SubstrateVM dependencies as part of it being
    * compiled with the SVM. But that has to be handled independently anyway.
    */
  val relevantConfigurations = Seq(Compile)

  /** Analyzes the provided [[SBTDistributionComponent]]s collecting their
    * unique dependencies and issuing any warnings.
    *
    * @param components description of SBT components included in the
    *                   distribution
    * @param log logger to use when resolving dependencies
    * @return a sequence of collected dependency information and a sequence of
    *         encountered warnings
    */
  def analyze(
    components: Seq[SBTDistributionComponent],
    log: ManagedLogger
  ): (Seq[DependencyInformation], Seq[Diagnostic]) = {
    val results: Seq[(Seq[Dependency], Vector[Path], Seq[Diagnostic])] =
      components.map { component =>
        val report = component.licenseReport.orig
        val ivyDeps =
          report.getDependencies.asScala.map(_.asInstanceOf[IvyNode])
        val sourceSuffix = "-sources.jar"
        val sourceArtifacts = component.classifiedArtifactsReport
          .select((configRef: ConfigRef) =>
            relevantConfigurations.map(_.name).contains(configRef.name)
          )
          .map(_.toPath)
          .filter(_.getFileName.toString.endsWith(sourceSuffix))
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
          val sources = sourceArtifacts.filter { src =>
            val fileName = src.getFileName.toString
            // Checking only the major version is a heuristic
            // Ignoring the version used to include too much: `http-auth` would also match sources of other package: `http-auth-spi`
            // However, exact version matches are too strict
            // (possibly because Maven resolves the versions based on the full dependency tree, possibly replacing the dependency?),
            // and resulted in missing sources. Major version match is a compromise between these two.
            val majorVersion   = dep.module.version.takeWhile(_ != '.')
            val expectedPrefix = dep.module.name + "-" + majorVersion
            fileName.stripSuffix(sourceSuffix).startsWith(expectedPrefix)
          }
          Dependency(dep, depNode, sources)
        }

        val diagnostics =
          if (component.licenseReport.licenses.isEmpty)
            Seq(
              Diagnostic.Error(
                s"License report for component ${component.name} is empty."
              )
            )
          else Seq()

        (deps, sourceArtifacts, diagnostics)
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
    } yield Diagnostic.Warning(s"Could not find sources for ${dep.moduleInfo}")
    val unexpectedWarnings = for {
      source <- distinctSources
      if !distinctDependencies.exists(_.sourcesJARPaths.contains(source))
    } yield Diagnostic.Warning(
      s"Found a source $source that does not belong to any known " +
      s"dependencies, perhaps the algorithm needs updating?"
    )
    val reportsWarnings = results.flatMap(_._3)

    (relevantDeps, missingWarnings ++ unexpectedWarnings ++ reportsWarnings)
  }

  /** Returns a project URL if it is defined for the dependency or None.
    */
  private def tryFindingUrl(dependency: Dependency): Option[String] =
    Option(dependency.ivyNode.getDescriptor).flatMap(descriptor =>
      Option(descriptor.getHomePage)
    )

  /** Creates a [[SourceAccess]] instance that unpacks the source files from a
    * JAR archive into a temporary directory.
    *
    * It removes the temporary directory after the analysis is finished.
    */
  private def createSourceAccessFromJAR(jarPath: Path): SourceAccess =
    new SourceAccess {
      override def access[R](withSources: Path => R): R =
        IO.withTemporaryDirectory { root =>
          IO.unzip(jarPath.toFile, root)
          withSources(root.toPath)
        }
    }

  /** Returns a sequence of [[SourceAccess]] instances that give access to any
    * sources JARs that are available with the dependency.
    */
  private def findSources(dependency: Dependency): Seq[SourceAccess] =
    dependency.sourcesJARPaths.map(createSourceAccessFromJAR)

  /** Wraps information related to a dependency.
    *
    * @param depLicense information on the license
    * @param ivyNode Ivy node that can be used to find metadata
    * @param sourcesJARPaths paths to JARs containing dependency's sources
    */
  case class Dependency(
    depLicense: DepLicense,
    ivyNode: IvyNode,
    sourcesJARPaths: Seq[Path]
  )

  /** Returns [[DepModuleInfo]] for an [[IvyNode]] if it is defined, or None.
    */
  def safeModuleInfo(dep: IvyNode): Option[DepModuleInfo] =
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
