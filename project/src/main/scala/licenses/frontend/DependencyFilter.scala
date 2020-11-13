package src.main.scala.licenses.frontend

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
    dependencyInformation.moduleInfo.organization != "org.enso"
}
