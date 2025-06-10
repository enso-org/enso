package org.enso.projectmanager.boot.configuration

/** A configuration object for metadata storage.
  *
  * @param projectMetadataDirectory a directory name containing project metadata
  * @param projectMetadataFileName a name of project metadata file
  */
case class MetadataStorageConfig(
  projectMetadataDirectory: String,
  projectMetadataFileName: String
)
