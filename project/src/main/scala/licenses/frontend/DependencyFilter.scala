package src.main.scala.licenses.frontend

import sbtlicensereport.license.DepModuleInfo
import src.main.scala.licenses.DependencyInformation

/** Filters out irrelevant dependencies.
  *
  * Currently, dependencies whose organisation is `org.enso` are ignored, as
  * they are owned by us, so they do not require any additional licensing
  * notices.
  */
object DependencyFilter {

  /** Decides if the dependency should be kept for further processing. */
  def shouldKeep(dependencyInformation: DependencyInformation): Boolean =
    shouldKeep(dependencyInformation.moduleInfo)

  /** Decides if the module should be kept for further processing. */
  def shouldKeep(moduleInfo: DepModuleInfo): Boolean =
    !shouldIgnore(moduleInfo)

  def shouldIgnore(moduleInfo: DepModuleInfo): Boolean = {
    val isEnsoModule = moduleInfo.organization == "org.enso"
    val isGuavaEmptyPlaceholder =
      moduleInfo.version == "9999.0-empty-to-avoid-conflict-with-guava"
    isEnsoModule || isGuavaEmptyPlaceholder
  }
}
