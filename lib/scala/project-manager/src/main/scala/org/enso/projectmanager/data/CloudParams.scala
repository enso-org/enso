package org.enso.projectmanager.data

/** Extra parameters required to run the project in hybrid mode.
  *
  * @param cloudProjectDirectoryPath the cloud project directory
  * @param cloudProjectId the cloud project id
  * @param cloudProjectSessionId the cloud project session id
  */
case class CloudParams(
  cloudProjectDirectoryPath: String,
  cloudProjectId: String,
  cloudProjectSessionId: String
)
