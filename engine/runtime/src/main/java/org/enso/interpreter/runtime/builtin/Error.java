package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/**
 * Container for builtin Error types
 */
public class Error {
  private final AtomConstructor syntaxError;
  private final AtomConstructor compileError;
  private final AtomConstructor inexhaustivePatternMatchError;
  private final AtomConstructor unitializedState;

  /**
   * Creates and registers the relevant constructors.
   *
   * @param language the current language instance.
   * @param scope the scope to register constructors in.
   */
  public Error(Language language, ModuleScope scope) {
    syntaxError =
        new AtomConstructor("Syntax_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "message", ArgumentDefinition.ExecutionMode.EXECUTE));
    compileError =
        new AtomConstructor("Compile_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "message", ArgumentDefinition.ExecutionMode.EXECUTE));
    inexhaustivePatternMatchError =
        new AtomConstructor("Inexhaustive_Pattern_Match_Error", scope)
            .initializeFields(
                new ArgumentDefinition(0, "scrutinee", ArgumentDefinition.ExecutionMode.EXECUTE));
    unitializedState =
        new AtomConstructor("Uninitialized_State", scope)
            .initializeFields(
                new ArgumentDefinition(0, "key", ArgumentDefinition.ExecutionMode.EXECUTE));

    scope.registerConstructor(syntaxError);
    scope.registerConstructor(compileError);
    scope.registerConstructor(inexhaustivePatternMatchError);
    scope.registerConstructor(unitializedState);
  }

  /** @return the builtin {@code Syntax_Error} atom constructor. */
  public AtomConstructor syntaxError() {
    return syntaxError;
  }

  /** @return the builtin {@code Compile_Error} atom constructor. */
  public AtomConstructor compileError() {
    return compileError;
  }

  /** @return the builtin {@code Inexhaustive_Pattern_Match_Error} atom constructor. */
  public AtomConstructor inexhaustivePatternMatchError() {
    return inexhaustivePatternMatchError;
  }

  /** @return the builtin {@code Uninitialized_State} atom constructor. */
  public AtomConstructor unitializedState() {
    return unitializedState;
  }
}
