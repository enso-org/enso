package src.main.scala.licenses.frontend

import com.typesafe.sbt.license.DepModuleInfo
import src.main.scala.licenses.DependencyInformation

/** Filters out irrelevant dependencies.
  *
  * Currently, dependencies whose organisation is `org.enso` are ignored, as
  * they are owned by us, so they do not require any additional licensing
  * notices.
  */
object DependencyFilter {

  /** Decides if the dependency should be kept for further processing.
    */
  def shouldKeep(dependencyInformation: DependencyInformation): Boolean =
    shouldKeep(dependencyInformation.moduleInfo)

  /** Decides if the module should be kept for further processing.
    */
  def shouldKeep(moduleInfo: DepModuleInfo): Boolean =
    moduleInfo.organization != "org.enso"
}
