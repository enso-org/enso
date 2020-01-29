package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.Source;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.dispatch.CallOptimiserNode;
import org.enso.interpreter.runtime.Builtins;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.type.Types;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;

import java.util.*;

/** A representation of Enso's per-file top-level scope. */
public class ModuleScope {
  private final AtomConstructor associatedType;
  private Map<String, AtomConstructor> constructors = new HashMap<>();
  private Map<AtomConstructor, Map<String, Function>> methods = new HashMap<>();
  private Set<ModuleScope> imports = new HashSet<>();

  /**
   * Creates a new object of this class.
   *
   * @param name the name of the newly created module.
   */
  public ModuleScope(String name) {
    associatedType = new AtomConstructor(name, this).initializeFields();
  }

  /**
   * Adds an Atom constructor definition to the module scope.
   *
   * @param constructor the constructor to register
   */
  public void registerConstructor(AtomConstructor constructor) {
    constructors.put(constructor.getName(), constructor);
  }

  /** @return the associated type of this module. */
  public AtomConstructor getAssociatedType() {
    return associatedType;
  }

  /**
   * Looks up a constructor in the module scope.
   *
   * @param name the name of the module binding
   * @return the Atom constructor associated with {@code name}, or {@link Optional#empty()}
   */
  public Optional<AtomConstructor> getConstructor(String name) {
    if (associatedType.getName().equals(name)) {
      return Optional.of(associatedType);
    }
    Optional<AtomConstructor> locallyDefined = Optional.ofNullable(this.constructors.get(name));
    if (locallyDefined.isPresent()) return locallyDefined;
    return imports.stream()
        .map(scope -> scope.getConstructor(name))
        .filter(Optional::isPresent)
        .map(Optional::get)
        .findFirst();
  }

  /**
   * Returns a map of methods defined in this module for a given constructor.
   *
   * @param cons the constructor for which method map is requested
   * @return a map containing all the defined methods by name
   */
  private Map<String, Function> ensureMethodMapFor(AtomConstructor cons) {
    return methods.computeIfAbsent(cons, k -> new HashMap<>());
  }

  private Map<String, Function> getMethodMapFor(AtomConstructor cons) {
    Map<String, Function> result = methods.get(cons);
    if (result == null) {
      return new HashMap<>();
    }
    return result;
  }

  /**
   * Registers a method defined for a given type.
   *
   * @param atom type the method was defined for
   * @param method method name
   * @param function the {@link Function} associated with this definition
   */
  public void registerMethod(AtomConstructor atom, String method, Function function) {
    ensureMethodMapFor(atom).put(method, function);
  }

  /**
   * Looks up the definition for a given type and method name.
   *
   * <p>The resolution algorithm is first looking for methods defined at the constructor definition
   * site (i.e. non-overloads), then looks for methods defined in this scope and finally tries to
   * resolve the method in all dependencies of this module.
   *
   * @param atom type to lookup the method for.
   * @param name the method name.
   * @return the matching method definition or null if not found.
   */
  @CompilerDirectives.TruffleBoundary
  public Function lookupMethodDefinition(AtomConstructor atom, String name) {
    Function definedWithAtom = atom.getDefinitionScope().getMethodMapFor(atom).get(name);
    if (definedWithAtom != null) {
      return definedWithAtom;
    }
    Function definedHere = getMethodMapFor(atom).get(name);
    if (definedHere != null) {
      return definedHere;
    }
    return imports.stream()
        .map(scope -> scope.getMethodMapFor(atom).get(name))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  /**
   * Adds a dependency for this module.
   *
   * @param scope the scope of the newly added dependency
   */
  public void addImport(ModuleScope scope) {
    imports.add(scope);
  }

  public Map<String, AtomConstructor> getConstructors() {
    return constructors;
  }

  public Map<AtomConstructor, Map<String, Function>> getMethods() {
    return methods;
  }

  public void reset() {
    imports = new HashSet<>();
    methods = new HashMap<>();
    constructors = new HashMap<>();
  }
}
