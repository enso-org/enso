package org.enso.projectmanager.model

import java.nio.file.Path

import org.enso.logger.{MaskingUtils, ToMaskedString}

/** The path to the project.
  *
  * @param path the project path value
  */
case class ProjectPath(path: Path) extends ToMaskedString {

  /** @inheritdoc */
  override def toMaskedString: String =
    MaskingUtils.toMaskedPath(path)
}
