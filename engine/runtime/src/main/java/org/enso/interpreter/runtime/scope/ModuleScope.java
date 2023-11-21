package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.RedefinedConversionException;
import org.enso.interpreter.runtime.error.RedefinedMethodException;

/** A representation of Enso's per-file top-level scope. */
public final class ModuleScope implements EnsoObject {
  private final Type associatedType;
  private final Module module;
  private Map<String, Object> polyglotSymbols;
  private Map<String, Type> types;
  private Map<Type, Map<String, Supplier<Function>>> methods;
  private Map<Type, Map<Type, Function>> conversions;
  private Set<ModuleScope> imports;
  private Set<ModuleScope> exports;

  private static final Type noTypeKey;

  static {
    noTypeKey = Type.noType();
  }

  /**
   * Creates a new object of this class.
   *
   * @param module the module related to the newly created scope.
   */
  public ModuleScope(Module module) {
    this.polyglotSymbols = new HashMap<>();
    this.types = new HashMap<>();
    this.methods = new ConcurrentHashMap<>();
    this.conversions = new ConcurrentHashMap<>();
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
      Map<Type, Map<String, Supplier<Function>>> methods,
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
  private Map<String, Supplier<Function>> ensureMethodMapFor(Type type) {
    Type tpeKey = type == null ? noTypeKey : type;
    return methods.computeIfAbsent(tpeKey, k -> new HashMap<>());
  }

  private Map<String, Supplier<Function>> getMethodMapFor(Type type) {
    Type tpeKey = type == null ? noTypeKey : type;
    Map<String, Supplier<Function>> result = methods.get(type);
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
    Map<String, Supplier<Function>> methodMap = ensureMethodMapFor(type);

    // Builtin types will have double definition because of
    // BuiltinMethod and that's OK
    if (methodMap.containsKey(method) && !type.isBuiltin()) {
      throw new RedefinedMethodException(type.getName(), method);
    } else {
      methodMap.put(method, new CachingSupplier<>(function));
    }
  }

  /**
   * Registers a lazily constructed method defined for a given type.
   *
   * @param type the type the method was defined for
   * @param method method name
   * @param supply provider of the {@link Function} associated with this definition
   */
  public void registerMethod(Type type, String method, Supplier<Function> supply) {
    Map<String, Supplier<Function>> methodMap = ensureMethodMapFor(type);

    // Builtin types will have double definition because of
    // BuiltinMethod and that's OK
    if (methodMap.containsKey(method) && !type.isBuiltin()) {
      throw new RedefinedMethodException(type.getName(), method);
    } else {
      methodMap.put(method, new CachingSupplier<>(supply));
    }
  }

  private static final class CachingSupplier<T> implements Supplier<T> {
    private final Supplier<T> supply;
    private T memo;

    CachingSupplier(Supplier<T> supply) {
      this.supply = supply;
    }

    CachingSupplier(T memo) {
      this.supply = null;
      this.memo = memo;
    }

    @Override
    public T get() {
      if (memo == null) {
        memo = supply.get();
      }
      return memo;
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
    var definedWithAtom = type.getDefinitionScope().getMethodMapFor(type).get(name);
    if (definedWithAtom != null) {
      return definedWithAtom.get();
    }

    var definedHere = getMethodMapFor(type).get(name);
    if (definedHere != null) {
      return definedHere.get();
    }

    return imports.stream()
        .map(scope -> scope.getExportedMethod(type, name))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  @TruffleBoundary
  public Function lookupConversionDefinition(Type original, Type target) {
    Function definedWithOriginal =
        original.getDefinitionScope().getConversionsFor(target).get(original);
    if (definedWithOriginal != null) {
      return definedWithOriginal;
    }
    Function definedWithTarget =
        target.getDefinitionScope().getConversionsFor(target).get(original);
    if (definedWithTarget != null) {
      return definedWithTarget;
    }
    Function definedHere = getConversionsFor(target).get(original);
    if (definedHere != null) {
      return definedHere;
    }
    return imports.stream()
        .map(scope -> scope.getExportedConversion(original, target))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  private Function getExportedMethod(Type type, String name) {
    var here = getMethodMapFor(type).get(name);
    if (here != null) {
      return here.get();
    }
    return exports.stream()
        .map(scope -> scope.getMethodMapFor(type).get(name))
        .filter(Objects::nonNull)
        .map(s -> s.get())
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

  /** @return a method for the given type */
  public Function getMethodForType(Type tpe, String name) {
    Type tpeKey = tpe == null ? noTypeKey : tpe;
    var allTpeMethods = methods.get(tpeKey);
    if (allTpeMethods == null) {
      return null;
    }
    var supply = allTpeMethods.get(name);
    return supply == null ? null : supply.get();
  }

  /**
   * Returns the names of methods for the given type.
   *
   * @param tpe the type in the scope
   * @return names of methods
   */
  public Set<String> getMethodNamesForType(Type tpe) {
    Type tpeKey = tpe == null ? noTypeKey : tpe;
    var allTpeMethods = methods.get(tpeKey);
    return allTpeMethods == null ? null : allTpeMethods.keySet();
  }

  /**
   * Registers all methods of a type in the provided scope.
   *
   * @param tpe the methods of which type should be registered
   * @param scope target scope where methods should be registered to
   */
  public void registerAllMethodsOfTypeToScope(Type tpe, ModuleScope scope) {
    Type tpeKey = tpe == null ? noTypeKey : tpe;
    var allTypeMethods = methods.get(tpeKey);
    if (allTypeMethods != null) {
      allTypeMethods.forEach((name, fun) -> scope.registerMethod(tpeKey, name, fun));
    }
  }

  /** @return methods for all registered types */
  public List<Function> getAllMethods() {
    return methods.values().stream()
        .flatMap(e -> e.values().stream())
        .map(s -> s.get())
        .collect(Collectors.toList());
  }

  /** @return the raw conversions held by this module */
  public List<Function> getConversions() {
    return conversions.values().stream()
        .flatMap(e -> e.values().stream())
        .collect(Collectors.toList());
  }

  /** @return the polyglot symbol imported into this scope. */
  public Object getPolyglotSymbol(String symbolName) {
    return polyglotSymbols.get(symbolName);
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
    Map<Type, Map<String, Supplier<Function>>> methods = new ConcurrentHashMap<>();
    Map<Type, Map<Type, Function>> conversions = new ConcurrentHashMap<>();
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
