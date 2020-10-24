package org.enso.componentmanager

import org.enso.cli.TaskProgress

trait UserInterface {
  def trackProgress(task: TaskProgress[_]): Unit
}
