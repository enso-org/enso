package org.enso.filewatcher.test

import org.enso.filewatcher.Watcher

import java.util.concurrent.Executor

/** A file watcher that does nothing. */
class NoopWatcher extends Watcher {

  /** @inheritdoc */
  override def start(ec: Executor): Unit = ()

  /** @inheritdoc */
  override def stop(): Unit = ()
}
