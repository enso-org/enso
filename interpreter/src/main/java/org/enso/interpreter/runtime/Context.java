package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleLanguage.Env;
import com.oracle.truffle.api.object.Layout;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import org.enso.interpreter.Language;

public class Context {
  private final Language language;
  private final Env environment;
  private final BufferedReader input;
  private final PrintWriter output;

  public Context() {
    language = null;
    environment = null;
    input = new BufferedReader(new InputStreamReader(System.in));
    output = new PrintWriter(System.out, true);
  }

  public Context(Language language, Env environment) {
    this.language = language;
    this.environment = environment;

    this.input = new BufferedReader(new InputStreamReader(environment.in()));
    this.output = new PrintWriter(environment.out(), true);
  }
}
