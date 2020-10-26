package org.enso.componentmanager.components

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress

trait ComponentManagementUserInterface {
  def trackProgress(task: TaskProgress[_]):                 Unit
  def shouldInstallBrokenEngine(version: SemVer):           Boolean
  def shouldInstallMissingEngine(version: SemVer):          Boolean
  def shouldInstallMissingRuntime(version: RuntimeVersion): Boolean
}
