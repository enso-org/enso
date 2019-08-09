package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.RootCallTarget;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import org.enso.interpreter.runtime.GlobalCallTarget;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/** A representation of Enso's top-level scope. */
public class GlobalScope {
  private final Map<String, GlobalCallTarget> globalNames = new HashMap<>();
  private final Map<String, AtomConstructor> constructors = new HashMap<>();

  /**
   * Creates a new instance of the global scope.
   *
   * This constructor will take on the duty of registering any built-in constructors for use by the
   * program.
   */
  public GlobalScope() {
    registerBuiltinConstructors();
  }

  /**
   * Registers any built-in Atom constructors under name in the global scope.
   */
  private void registerBuiltinConstructors() {
    registerConstructor(AtomConstructor.UNIT);
    registerConstructor(AtomConstructor.NIL);
    registerConstructor(AtomConstructor.CONS);
  }

  /**
   * Adds an Atom constructor definition to the global scope.
   *
   * @param constructor the constructor to register
   */
  public void registerConstructor(AtomConstructor constructor) {
    constructors.put(constructor.getName(), constructor);
  }

  /**
   * Registers a new name for a call target in the global scope.
   *
   * @param name the name of the global variable
   */
  public void registerName(String name) {
    this.globalNames.put(name, new GlobalCallTarget(null));
  }

  /**
   * Assigns a value to an existing global variable.
   *
   * @param name the name to assign to
   * @param rootBinding the code to bind to that name
   */
  public void updateCallTarget(String name, RootCallTarget rootBinding) {
    GlobalCallTarget globalTarget = this.globalNames.get(name);

    if (globalTarget == null) {
      this.globalNames.put(name, new GlobalCallTarget(rootBinding));
    } else {
      globalTarget.setTarget(rootBinding);
    }
  }

  /**
   * Looks up a call target in the global scope.
   *
   * @param name the name of the global binding
   * @return the call target associated with {@code name}, or {@link Optional#empty()}
   */
  public Optional<GlobalCallTarget> getGlobalCallTarget(String name) {
    return Optional.ofNullable(this.globalNames.get(name));
  }

  /**
   * Looks up a constructor in the global scope.
   *
   * @param name the name of the global binding
   * @return the Atom constructor associated with {@code name}, or {@link Optional#empty()}
   */
  public Optional<AtomConstructor> getConstructor(String name) {
    return Optional.ofNullable(this.constructors.get(name));
  }
}
