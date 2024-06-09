package org.enso.filewatcher

import java.util.concurrent.Executor

/** A file watcher that does nothing. */
class NoopWatcher extends Watcher {

  /** @inheritdoc */
  override def start(ec: Executor): Unit = ()

  /** @inheritdoc */
  override def stop(): Unit = ()
}
