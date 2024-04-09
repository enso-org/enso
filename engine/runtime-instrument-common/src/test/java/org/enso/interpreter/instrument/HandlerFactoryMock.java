package org.enso.interpreter.instrument;

@org.openide.util.lookup.ServiceProvider(service = HandlerFactory.class)
public class HandlerFactoryMock extends HandlerFactory {
  @Override
  public Handler create() {
    return new MockHandler();
  }
}
