package org.enso.filewatcher

/** A file watcher that does nothing. */
class NoopWatcher extends Watcher {

  /** @inheritdoc */
  override def start(): Unit = ()

  /** @inheritdoc */
  override def stop(): Unit = ()
}
