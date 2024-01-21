package org.enso.interpreter.instrument

object HandlerFactoryImpl extends HandlerFactory {
  override def create(): Handler = new HandlerImpl()
}
