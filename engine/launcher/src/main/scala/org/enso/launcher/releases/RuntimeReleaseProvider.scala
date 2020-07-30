package org.enso.launcher.releases

import java.nio.file.Path

import org.enso.cli.TaskProgress
import org.enso.launcher.components.RuntimeVersion

trait RuntimeReleaseProvider {
  def packageFileName(version: RuntimeVersion): String

  def downloadPackage(
    version: RuntimeVersion,
    path: Path
  ): TaskProgress[Unit]
}
