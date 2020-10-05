package src.main.scala.licenses.frontend

import src.main.scala.licenses.DependencyInformation

object DependencyFilter {
  def shouldKeep(dependencyInformation: DependencyInformation): Boolean =
    dependencyInformation.moduleInfo.organization != "org.enso"
}
