package org.enso.languageserver.boot

/** Signal where the lang. server is deployed. */
sealed trait DeploymentType

object DeploymentType {

  /** Desktop deployment. */
  case object Desktop extends DeploymentType

  /** Azure deployment. */
  case object Azure extends DeploymentType

  /** Determines the current deployment type from environment variables.
    * @return the current deployment type
    */
  def fromEnvironment(): DeploymentType = {
    if (sys.env.contains(DeploymentTypeVariableName)) {
      val value = sys.env(DeploymentTypeVariableName)
      fromString(value)
    } else {
      Desktop
    }
  }

  /** Determines a current deployment type from a string value.
    * @return a deployment type
    */
  def fromString(value: String): DeploymentType =
    value.toLowerCase.trim match {
      case "desktop" | "" => Desktop
      case "azure"        => Azure
    }

  private lazy val DeploymentTypeVariableName = "DEPLOYMENT_TYPE"

}
