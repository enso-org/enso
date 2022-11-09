package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.UniquelyConstructibleBuiltin;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.struct.Struct;

import java.io.IOException;
import java.time.DateTimeException;
import java.time.format.DateTimeParseException;
import java.util.List;

@BuiltinType
public class PolyglotError extends UniquelyConstructibleBuiltin {
  @Override
  protected List<String> getConstructorParamNames() {
    return List.of("cause");
  }

  public Struct wrap(AbstractTruffleException e) {
    return newInstance(e);
  }

  public Struct wrap(Context c, IOException e) {
    return newInstance(c.getEnvironment().asGuestValue(e));
  }

  public Struct wrap(Context c, DateTimeException e) {
    return newInstance(c.getEnvironment().asGuestValue(e));
  }

  public Struct wrap(Context c, DateTimeParseException e) {
    return newInstance(c.getEnvironment().asGuestValue(e));
  }
}
