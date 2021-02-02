package org.enso.interpreter.epb;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;

public class EpbParser {
  public static class Result {
    private final String foreignSource;
    private final @CompilerDirectives.CompilationFinal String[] arguments;

    public Result(String foreignSource, String[] arguments) {
      this.foreignSource = foreignSource;
      this.arguments = arguments;
    }

    public String getForeignSource() {
      return foreignSource;
    }

    public String[] getArguments() {
      return arguments;
    }
  }

  public Result run(TruffleLanguage.ParsingRequest request) {
    return new Result(
        request.getSource().getCharacters().toString(),
        request.getArgumentNames().toArray(new String[0]));
  }
}
