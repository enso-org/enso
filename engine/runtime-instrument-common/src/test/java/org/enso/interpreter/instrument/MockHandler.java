package org.enso.interpreter.instrument;

import org.enso.interpreter.instrument.command.CommandFactory;
import org.enso.interpreter.instrument.command.MockedCommandFactory;

public class MockHandler extends Handler {

  private CommandFactory _cmdFactory = new MockedCommandFactory();

  @Override
  public CommandFactory cmdFactory() {
    return _cmdFactory;
  }
}
