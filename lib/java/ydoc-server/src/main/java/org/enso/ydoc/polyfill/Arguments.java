package org.enso.ydoc.polyfill;

import java.util.Arrays;
import org.graalvm.polyglot.Value;

public final class Arguments {

  private Arguments() {}

  public static String toString(Value... arguments) {
    var stringArguments =
        Arrays.stream(arguments)
            .map(argument -> argument.isString() ? argument.asString() : argument.toString())
            .toArray(String[]::new);
    return Arrays.toString(stringArguments);
  }
}
