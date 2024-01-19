package org.enso.interpreter.instrument
import org.enso.interpreter.instrument.command.MockedCommandFactory

@org.openide.util.lookup.ServiceProvider(service = classOf[Handler])
class MockHandler extends Handler {
  override lazy val cmdFactory = new MockedCommandFactory
}
