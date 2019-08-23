package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.scope.GlobalScope;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;

/**
 * The language context is the internal state of the language that is associated with each thread in
 * a running Enso program.
 */
public class Context {
  private final Language language;
  private final Env environment;
  private final BufferedReader input;
  private final PrintWriter output;
  private final GlobalScope globalScope;

  /**
   * Creates a new Enso context.
   *
   * @param language the language identifier
   * @param environment the execution environment of the {@link TruffleLanguage}
   */
  public Context(Language language, Env environment) {
    this.language = language;
    this.environment = environment;

    this.globalScope = new GlobalScope();
    this.input = new BufferedReader(new InputStreamReader(environment.in()));
    this.output = new PrintWriter(environment.out(), true);
  }

  /**
   * Obtains a reference to the Enso global scope.
   *
   * @return the Enso global scope
   */
  public GlobalScope getGlobalScope() {
    return globalScope;
  }
}
