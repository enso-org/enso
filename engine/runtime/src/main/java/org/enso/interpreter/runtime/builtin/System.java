package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.system.CreateProcessMethodGen;
import org.enso.interpreter.node.expression.builtin.system.ExitMethodGen;
import org.enso.interpreter.node.expression.builtin.system.NanoTimeMethodGen;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container class for all System-related stdlib builtins. */
public class System {

  private final AtomConstructor system;
  private final AtomConstructor processResult;
  private final AtomConstructor processBuilder;

  /**
   * Create and register all {@code System} constructors.
   *
   * @param language the current language instance.
   * @param scope the scope to register constructors and methods in.
   */
  public System(Language language, ModuleScope scope) {
    system = new AtomConstructor("System", scope).initializeFields();
    scope.registerConstructor(system);
    processResult =
        new AtomConstructor("Process_Result", scope)
            .initializeFields(
                new ArgumentDefinition(0, "exit_code", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "stdout", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(2, "stderr", ArgumentDefinition.ExecutionMode.EXECUTE));
    scope.registerConstructor(processResult);
    processBuilder =
        new AtomConstructor("Process_Builder", scope)
            .initializeFields(
                new ArgumentDefinition(0, "command", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "arguments", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(2, "stdin", ArgumentDefinition.ExecutionMode.EXECUTE));

    scope.registerMethod(system, "create_process", CreateProcessMethodGen.makeFunction(language));
    scope.registerMethod(system, "nano_time", NanoTimeMethodGen.makeFunction(language));
    scope.registerMethod(system, "exit", ExitMethodGen.makeFunction(language));
  }

  /** @return the atom constructor for {@code System}. */
  public AtomConstructor getSystem() {
    return system;
  }

  /** @return the atom constructor for {@code Process_Result}. */
  public AtomConstructor getProcessResult() {
    return processResult;
  }

  /** @return the atom constructor for {@code Process_Builder}. */
  public AtomConstructor getProcessBuilder() {
    return processBuilder;
  }
}
