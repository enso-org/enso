package org.enso.interpreter.runtime.scope;

import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/** A representation of Enso's top-level scope. */
public class GlobalScope {
  private final Map<String, AtomConstructor> constructors = new HashMap<>();
  private final Map<AtomConstructor, Map<String, Function>> methods = new HashMap<>();

  /**
   * Creates a new instance of the global scope.
   *
   * <p>This constructor will take on the duty of registering any built-in constructors for use by
   * the program.
   */
  public GlobalScope() {
    registerBuiltinConstructors();
  }

  /** Registers any built-in Atom constructors under name in the global scope. */
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
   * Looks up a constructor in the global scope.
   *
   * @param name the name of the global binding
   * @return the Atom constructor associated with {@code name}, or {@link Optional#empty()}
   */
  public Optional<AtomConstructor> getConstructor(String name) {
    return Optional.ofNullable(this.constructors.get(name));
  }


  private Map<String, Function> getMethodMapFor(AtomConstructor atom) {
    Map<String, Function> result = methods.get(atom);
    if (result == null) {
      result = new HashMap<>();
      methods.put(atom, result);
    }
    return result;
  }

  /**
   * Registers a method defined for a given type.
   * @param atom type the method was defined for.
   * @param method method name.
   * @param function the {@link Function} associated with this definition.
   */
  public void registerMethod(AtomConstructor atom, String method, Function function) {
    getMethodMapFor(atom).put(method, function);
  }

  /**
   * Looks up the definition for a given type and method name.
   * @param atom type to lookup the method for.
   * @param name the method name.
   * @return the matching method definition or null if not found.
   */
  public Function lookupMethodDefinition(AtomConstructor atom, String name) {
    return getMethodMapFor(atom).get(name);
  }
}
