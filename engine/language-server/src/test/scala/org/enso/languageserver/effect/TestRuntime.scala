package org.enso.languageserver.effect

/** Runtime that is used in tests. */
final class TestRuntime extends Runtime {

  /** @inheritdoc */
  override def instance: zio.Runtime[zio.ZAny] =
    zio.Runtime.default

  /** @inheritdoc */
  override def init(): Unit = ()
}
