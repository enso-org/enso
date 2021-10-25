package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.system.CreateProcessMethodGen;
import org.enso.interpreter.node.expression.builtin.system.ExitMethodGen;
import org.enso.interpreter.node.expression.builtin.system.NanoTimeMethodGen;
import org.enso.interpreter.node.expression.builtin.system.OsMethodGen;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container class for all System-related stdlib builtins. */
public class System {

  private final AtomConstructor system;
  private final AtomConstructor systemProcessResult;

  /**
   * Create and register all {@code System} constructors.
   *
   * @param language the current language instance.
   * @param scope the scope to register constructors and methods in.
   */
  public System(Language language, ModuleScope scope) {
    system = new AtomConstructor("System", scope).initializeFields();
    scope.registerConstructor(system);
    systemProcessResult =
        new AtomConstructor("System_Process_Result", scope)
            .initializeFields(
                new ArgumentDefinition(0, "exit_code", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(1, "stdout", ArgumentDefinition.ExecutionMode.EXECUTE),
                new ArgumentDefinition(2, "stderr", ArgumentDefinition.ExecutionMode.EXECUTE));
    scope.registerConstructor(systemProcessResult);

    scope.registerMethod(system, "create_process", CreateProcessMethodGen.makeFunction(language));
    scope.registerMethod(system, "nano_time", NanoTimeMethodGen.makeFunction(language));
    scope.registerMethod(system, "exit", ExitMethodGen.makeFunction(language));
    scope.registerMethod(system, "os", OsMethodGen.makeFunction(language));
  }

  /** @return the atom constructor for {@code System}. */
  public AtomConstructor getSystem() {
    return system;
  }

  /** @return the atom constructor for {@code Process_Result}. */
  public AtomConstructor getSystemProcessResult() {
    return systemProcessResult;
  }
}
