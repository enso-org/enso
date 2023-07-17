package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.TruffleObject;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.RedefinedConversionException;
import org.enso.interpreter.runtime.error.RedefinedMethodException;

/** A representation of Enso's per-file top-level scope. */
public final class ModuleScope implements TruffleObject {
  private final Type associatedType;
  private final Module module;
  private Map<String, Object> polyglotSymbols;
  private Map<String, Type> types;
  private Map<Type, Map<String, Function>> methods;
  private Map<Type, Map<Type, Function>> conversions;
  private Set<ModuleScope> imports;
  private Set<ModuleScope> exports;

  /**
   * Creates a new object of this class.
   *
   * @param module the module related to the newly created scope.
   */
  public ModuleScope(Module module) {
    this.polyglotSymbols = new HashMap<>();
    this.types = new HashMap<>();
    this.methods = new HashMap<>();
    this.conversions = new HashMap<>();
    this.imports = new HashSet<>();
    this.exports = new HashSet<>();
    this.module = module;
    this.associatedType = Type.createSingleton(module.getName().item(), this, null, false);
  }

  public ModuleScope(
      Module module,
      Type associatedType,
      Map<String, Object> polyglotSymbols,
      Map<String, Type> types,
      Map<Type, Map<String, Function>> methods,
      Map<Type, Map<Type, Function>> conversions,
      Set<ModuleScope> imports,
      Set<ModuleScope> exports) {
    this.module = module;
    this.associatedType = associatedType;
    this.polyglotSymbols = polyglotSymbols;
    this.types = types;
    this.methods = methods;
    this.conversions = conversions;
    this.imports = imports;
    this.exports = exports;
  }

  public void registerType(Type type) {
    types.put(type.getName(), type);
  }

  /** @return the associated type of this module. */
  public Type getAssociatedType() {
    return associatedType;
  }

  /** @return the module associated with this scope. */
  public Module getModule() {
    return module;
  }

  /** @return the set of modules imported by this module. */
  public Set<ModuleScope> getImports() {
    return imports;
  }

  /**
   * Returns a map of methods defined in this module for a given constructor.
   *
   * @param type the type for which method map is requested
   * @return a map containing all the defined methods by name
   */
  private Map<String, Function> ensureMethodMapFor(Type type) {
    return methods.computeIfAbsent(type, k -> new HashMap<>());
  }

  private Map<String, Function> getMethodMapFor(Type type) {
    Map<String, Function> result = methods.get(type);
    if (result == null) {
      return new HashMap<>();
    }
    return result;
  }

  /**
   * Registers a method defined for a given type.
   *
   * @param type the type the method was defined for
   * @param method method name
   * @param function the {@link Function} associated with this definition
   */
  public void registerMethod(Type type, String method, Function function) {
    Map<String, Function> methodMap = ensureMethodMapFor(type);

    // Builtin types will have double definition because of
    // BuiltinMethod and that's OK
    if (methodMap.containsKey(method) && !type.isBuiltin()) {
      throw new RedefinedMethodException(type.getName(), method);
    } else {
      methodMap.put(method, function);
    }
  }

  /**
   * Returns a list of the conversion methods defined in this module for a given constructor.
   *
   * @param type the type for which method map is requested
   * @return a list containing all the defined conversions in definition order
   */
  private Map<Type, Function> ensureConversionsFor(Type type) {
    return conversions.computeIfAbsent(type, k -> new HashMap<>());
  }

  private Map<Type, Function> getConversionsFor(Type type) {
    var result = conversions.get(type);
    if (result == null) {
      return new HashMap<>();
    }
    return result;
  }

  /**
   * Registers a conversion method for a given type
   *
   * @param toType type the conversion was defined to
   * @param fromType type the conversion was defined from
   * @param function the {@link Function} associated with this definition
   */
  public void registerConversionMethod(Type toType, Type fromType, Function function) {
    var sourceMap = ensureConversionsFor(toType);
    if (sourceMap.containsKey(fromType)) {
      throw new RedefinedConversionException(toType.getName(), fromType.getName());
    } else {
      sourceMap.put(fromType, function);
    }
  }

  /**
   * Registers a new symbol in the polyglot namespace.
   *
   * @param name the name of the symbol
   * @param sym the value being exposed
   */
  public void registerPolyglotSymbol(String name, Object sym) {
    polyglotSymbols.put(name, sym);
  }

  /**
   * Looks up the definition for a given type and method name.
   *
   * <p>The resolution algorithm is first looking for methods defined at the constructor definition
   * site (i.e. non-overloads), then looks for methods defined in this scope and finally tries to
   * resolve the method in all dependencies of this module.
   *
   * @param type type to lookup the method for.
   * @param name the method name.
   * @return the matching method definition or null if not found.
   */
  @TruffleBoundary
  public Function lookupMethodDefinition(Type type, String name) {
    Function definedWithAtom = type.getDefinitionScope().getMethodMapFor(type).get(name);
    if (definedWithAtom != null) {
      return definedWithAtom;
    }

    Function definedHere = getMethodMapFor(type).get(name);
    if (definedHere != null) {
      return definedHere;
    }

    return imports.stream()
        .map(scope -> scope.getExportedMethod(type, name))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  @TruffleBoundary
  public Function lookupConversionDefinition(Type type, Type target) {
    Function definedWithAtom = type.getDefinitionScope().getConversionsFor(target).get(type);
    if (definedWithAtom != null) {
      return definedWithAtom;
    }
    Function definedHere = getConversionsFor(target).get(type);
    if (definedHere != null) {
      return definedHere;
    }
    return imports.stream()
        .map(scope -> scope.getExportedConversion(type, target))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  private Function getExportedMethod(Type type, String name) {
    Function here = getMethodMapFor(type).get(name);
    if (here != null) {
      return here;
    }
    return exports.stream()
        .map(scope -> scope.getMethodMapFor(type).get(name))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  private Function getExportedConversion(Type type, Type target) {
    Function here = getConversionsFor(target).get(type);
    if (here != null) {
      return here;
    }
    return exports.stream()
        .map(scope -> scope.getConversionsFor(target).get(type))
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

  /**
   * Adds an information about the module exporting another module.
   *
   * @param scope the exported scope
   */
  public void addExport(ModuleScope scope) {
    exports.add(scope);
  }

  public Map<String, Type> getTypes() {
    return types;
  }

  public Optional<Type> getType(String name) {
    if (associatedType.getName().equals(name)) {
      return Optional.of(associatedType);
    }
    return Optional.ofNullable(types.get(name));
  }

  /** @return the raw method map held by this module */
  public Map<Type, Map<String, Function>> getMethods() {
    return methods;
  }

  /** @return the raw conversions map held by this module */
  public Map<Type, Map<Type, Function>> getConversions() {
    return conversions;
  }

  /** @return the polyglot symbols imported into this scope. */
  public Map<String, Object> getPolyglotSymbols() {
    return polyglotSymbols;
  }

  public void reset() {
    imports = new HashSet<>();
    exports = new HashSet<>();
    methods = new HashMap<>();
    types = new HashMap<>();
    conversions = new HashMap<>();
    polyglotSymbols = new HashMap<>();
  }

  /**
   * Create a copy of this `ModuleScope` while taking into account only the provided list of types.
   *
   * @param typeNames list of types to copy to the new scope
   * @return a copy of this scope modulo the requested types
   */
  public ModuleScope withTypes(List<String> typeNames) {
    Map<String, Object> polyglotSymbols = new HashMap<>(this.polyglotSymbols);
    Map<String, Type> requestedTypes = new HashMap<>(this.types);
    Map<Type, Map<String, Function>> methods = new HashMap<>();
    Map<Type, Map<Type, Function>> conversions = new HashMap<>();
    Set<ModuleScope> imports = new HashSet<>(this.imports);
    Set<ModuleScope> exports = new HashSet<>(this.exports);
    this.types
        .entrySet()
        .forEach(
            entry -> {
              if (typeNames.contains(entry.getKey())) {
                requestedTypes.put(entry.getKey(), entry.getValue());
              }
            });
    Collection<Type> validTypes = requestedTypes.values();
    this.methods.forEach(
        (tpe, meths) -> {
          if (validTypes.contains(tpe)) {
            methods.put(tpe, meths);
          }
        });
    this.conversions.forEach(
        (tpe, meths) -> {
          if (validTypes.contains(tpe)) {
            conversions.put(tpe, meths);
          }
        });

    return new ModuleScope(
        module,
        associatedType,
        polyglotSymbols,
        requestedTypes,
        methods,
        conversions,
        imports,
        exports);
  }

  @Override
  public String toString() {
    return "Scope" + module;
  }
}
