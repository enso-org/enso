package org.enso.interpreter.instrument

private object HandlerFactoryImpl extends HandlerFactory {
  override def create(): Handler = new HandlerImpl()
}
