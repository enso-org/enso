package org.enso.interpreter.instrument;

import org.enso.interpreter.instrument.command.CommandFactory;
import org.enso.interpreter.instrument.command.MockedCommandFactory;
@org.openide.util.lookup.ServiceProvider(service = Handler.class)
public class MockHandler extends Handler {

    CommandFactory _cmdFactory = new MockedCommandFactory();

    @Override
    public CommandFactory cmdFactory() {
        System.out.println("Cmad factory: mock");
        return _cmdFactory;
    }
}
