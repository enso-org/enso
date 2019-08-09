package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;

/** A class that represents all runtime types in Enso that can be called. */
public abstract class Callable {
  private @CompilationFinal(dimensions = 1) ArgumentDefinition[] args;

  /**
   * Creates a new callable with the specified arguments.
   *
   * @param args information on the arguments defined for the callable, in the order they are
   *     defined
   */
  public Callable(ArgumentDefinition[] args) {
    this.args = args;
  }

  /**
   * Gets the arguments defined on this callable.
   *
   * @return information on the arguments defined for the callable
   */
  public ArgumentDefinition[] getArgs() {
    return this.args;
  }
}
