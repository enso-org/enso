package org.enso.cli.task

/** Defines how to inform the user about progress of long running tasks. */
trait ProgressReporter {

  /** Called when a long-running task is started.
    *
    * Can be used to track its progress.
    *
    * @param message message associated with the task
    * @param task the long running task to track
    */
  def trackProgress(message: String, task: TaskProgress[_]): Unit
}
