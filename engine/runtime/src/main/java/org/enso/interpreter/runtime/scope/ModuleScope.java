package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
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
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.util.CachingSupplier;

/** A representation of Enso's per-file top-level scope. */
@ExportLibrary(TypesLibrary.class)
public final class ModuleScope implements EnsoObject {
  private final Type associatedType;
  private final Module module;
  private final Map<String, Object> polyglotSymbols;
  private final Map<String, Type> types;
  private final Map<Type, Map<String, Supplier<Function>>> methods;
  private final Map<Type, Map<Type, Function>> conversions;
  private final Set<ModuleScope.Builder> imports;
  private final Set<ModuleScope.Builder> exports;

  private static final Type noTypeKey;

  private static int intGen = 0;

  static {
    noTypeKey = Type.noType();
  }

  private final int id;

  public ModuleScope(
      Module module,
      Type associatedType,
      Map<String, Object> polyglotSymbols,
      Map<String, Type> types,
      Map<Type, Map<String, Supplier<Function>>> methods,
      Map<Type, Map<Type, Function>> conversions,
      Set<ModuleScope.Builder> imports,
      Set<ModuleScope.Builder> exports) {
    this.module = module;
    this.associatedType = associatedType;
    this.polyglotSymbols = polyglotSymbols;
    this.types = types;
    this.methods = methods;
    this.conversions = conversions;
    this.imports = imports;
    this.exports = exports;
    this.id = intGen++;
  }

  /**
   * @return the associated type of this module.
   */
  public Type getAssociatedType() {
    return associatedType;
  }

  /**
   * @return the module associated with this scope.
   */
  public Module getModule() {
    return module;
  }

  private Map<String, Supplier<Function>> getMethodMapFor(Type type) {
    Type tpeKey = type == null ? noTypeKey : type;
    Map<String, Supplier<Function>> result = methods.get(tpeKey);
    if (result == null) {
      return new HashMap<>();
    }
    return result;
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
  @CompilerDirectives.TruffleBoundary
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
        .map(scope -> scope.build().getExportedMethod(type, name))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  @CompilerDirectives.TruffleBoundary
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
        .map(scope -> scope.build().getExportedConversion(original, target))
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
        .map(scope -> scope.build().getMethodMapFor(type).get(name))
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
        .map(scope -> scope.build().getConversionsFor(target).get(type))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  public Map<String, Type> getTypes() {
    return types;
  }

  @ExportMessage.Ignore
  public Optional<Type> getType(String name) {
    if (associatedType.getName().equals(name)) {
      return Optional.of(associatedType);
    }
    return Optional.ofNullable(types.get(name));
  }

  /**
   * @return a method for the given type
   */
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
   * @param tpe the type in the scope. If null, treated as {@code noType}.
   * @return names of methods or null
   */
  public Set<String> getMethodNamesForType(Type tpe) {
    Type tpeKey = tpe == null ? noTypeKey : tpe;
    var allTpeMethods = methods.get(tpeKey);
    return allTpeMethods == null ? null : allTpeMethods.keySet();
  }

  /**
   * Returns a set of all the functions for a type, or null.
   *
   * @param tpe the type in the scope. If null, treated as {@code noType}.
   * @return set of methods or null.
   */
  public Set<Function> getMethodsForType(Type tpe) {
    Type tpeKey = tpe == null ? noTypeKey : tpe;
    var allTpeMethods = methods.get(tpeKey);
    if (allTpeMethods != null) {
      return allTpeMethods.values().stream().map(Supplier::get).collect(Collectors.toSet());
    } else {
      return null;
    }
  }

  private Map<Type, Function> getConversionsFor(Type type) {
    var result = conversions.get(type);
    if (result == null) {
      return new HashMap<>();
    }
    return result;
  }

  /**
   * @return methods for all registered types
   */
  public List<Function> getAllMethods() {
    return methods.values().stream()
        .flatMap(e -> e.values().stream())
        .map(s -> s.get())
        .collect(Collectors.toList());
  }

  /**
   * @return the raw conversions held by this module
   */
  public List<Function> getConversions() {
    return conversions.values().stream()
        .flatMap(e -> e.values().stream())
        .collect(Collectors.toList());
  }

  /**
   * @return the polyglot symbol imported into this scope.
   */
  public Object getPolyglotSymbol(String symbolName) {
    return polyglotSymbols.get(symbolName);
  }

  // FIXME: this is wrong
  /**
   * Create a copy of this `ModuleScope` while taking into account only the provided list of types.
   *
   * @param typeNames list of types to copy to the new scope
   * @return a copy of this scope modulo the requested types
   */
  public ModuleScope.Builder withTypes(List<String> typeNames) {
    Map<String, Object> polyglotSymbols = new HashMap<>(this.polyglotSymbols);
    Map<String, Type> requestedTypes = new HashMap<>(this.types);
    Map<Type, Map<String, Supplier<Function>>> methods = new ConcurrentHashMap<>();
    Map<Type, Map<Type, Function>> conversions = new ConcurrentHashMap<>();
    Set<ModuleScope.Builder> imports = new HashSet<>(this.imports);
    Set<ModuleScope.Builder> exports = new HashSet<>(this.exports);
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

    return new ModuleScope.Builder(
        module,
        associatedType,
        polyglotSymbols,
        requestedTypes,
        methods,
        conversions,
        imports,
        exports);
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType() {
    return getAssociatedType();
  }

  @Override
  public String toString() {
    return "Scope" + module + "-" + id;
  }

  public static class Builder {

    @CompilerDirectives.CompilationFinal private volatile ModuleScope moduleScope = null;

    private final Module module;
    private final Type associatedType;
    private Map<String, Object> polyglotSymbols;
    private Map<String, Type> types;
    private Map<Type, Map<String, Supplier<Function>>> methods;
    private Map<Type, Map<Type, Function>> conversions;
    private Set<ModuleScope.Builder> imports;
    private Set<ModuleScope.Builder> exports;

    public Builder(Module module) {
      this.module = module;
      this.polyglotSymbols = new HashMap<>();
      this.types = new HashMap<>();
      this.methods = new ConcurrentHashMap<>();
      this.conversions = new ConcurrentHashMap<>();
      this.imports = new HashSet<>();
      this.exports = new HashSet<>();
      this.associatedType = Type.createSingleton(module.getName().item(), this, null, false, false);
    }

    public Builder(
        Module module,
        Type associatedType,
        Map<String, Object> polyglotSymbols,
        Map<String, Type> types,
        Map<Type, Map<String, Supplier<Function>>> methods,
        Map<Type, Map<Type, Function>> conversions,
        Set<ModuleScope.Builder> imports,
        Set<ModuleScope.Builder> exports) {
      this.module = module;
      this.associatedType = associatedType;
      this.polyglotSymbols = polyglotSymbols;
      this.types = types;
      this.methods = methods;
      this.conversions = conversions;
      this.imports = imports;
      this.exports = exports;
    }

    public Type registerType(Type type) {
      assert moduleScope == null;
      Type current = types.putIfAbsent(type.getName(), type);
      return current == null ? type : current;
    }

    /**
     * Returns a map of methods defined in this module for a given type.
     *
     * @param type the type for which method map is requested
     * @return a map containing all the defined methods by name
     */
    private Map<String, Supplier<Function>> ensureMethodMapFor(Type type) {
      Type tpeKey = type == null ? noTypeKey : type;
      return methods.computeIfAbsent(tpeKey, k -> new HashMap<>());
    }

    /**
     * Registers a method defined for a given type.
     *
     * @param type the type the method was defined for
     * @param method method name
     * @param function the {@link Function} associated with this definition
     */
    public void registerMethod(Type type, String method, Function function) {
      assert moduleScope == null;
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
      if (moduleScope != null) {
        System.out.println("What module? " + module.getName());
      }
      assert moduleScope == null;
      Map<String, Supplier<Function>> methodMap = ensureMethodMapFor(type);

      // Builtin types will have double definition because of
      // BuiltinMethod and that's OK
      if (methodMap.containsKey(method) && !type.isBuiltin()) {
        throw new RedefinedMethodException(type.getName(), method);
      } else {
        methodMap.put(method, new CachingSupplier<>(supply));
      }
    }

    /**
     * Registers a conversion method for a given type
     *
     * @param toType type the conversion was defined to
     * @param fromType type the conversion was defined from
     * @param function the {@link Function} associated with this definition
     */
    public void registerConversionMethod(Type toType, Type fromType, Function function) {
      assert moduleScope == null;
      var sourceMap = conversions.computeIfAbsent(toType, k -> new HashMap<>());
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
      assert moduleScope == null;
      polyglotSymbols.put(name, sym);
    }

    /**
     * Registers all methods of a type in the provided scope.
     *
     * @param tpe the methods of which type should be registered
     * @param scope target scope where methods should be registered to
     */
    public void registerAllMethodsOfTypeToScope(Type tpe, ModuleScope.Builder scope) {
      // FIXME: because of Builtins can't enable 'assert moduleScope == null;'
      Type tpeKey = tpe == null ? noTypeKey : tpe;
      var allTypeMethods = methods.get(tpeKey);
      if (allTypeMethods != null) {
        allTypeMethods.forEach((name, fun) -> scope.registerMethod(tpeKey, name, fun));
      }
    }

    /**
     * Adds a dependency for this module.
     *
     * @param scope the scope of the newly added dependency
     */
    public void addImport(ModuleScope.Builder scope) {
      assert moduleScope == null;
      imports.add(scope);
    }

    /**
     * Adds an information about the module exporting another module.
     *
     * @param scope the exported scope
     */
    public void addExport(ModuleScope.Builder scope) {
      assert moduleScope == null;
      exports.add(scope);
    }

    public Module getModule() {
      return module;
    }

    public Type getType(String typeName) {
      // assert moduleScope == null;
      return types.get(typeName);
    }

    /**
     * @return the associated type of this module.
     */
    public Type getAssociatedType() {
      return associatedType;
    }

    public Object getPolyglotSymbol(String symbolName) {
      return polyglotSymbols.get(symbolName);
    }

    public ModuleScope.Builder withTypes(List<String> typeNames) {
      Map<String, Object> polyglotSymbols = new HashMap<>(this.polyglotSymbols);
      Map<String, Type> requestedTypes = new HashMap<>(this.types);
      Map<Type, Map<String, Supplier<Function>>> methods = new ConcurrentHashMap<>();
      Map<Type, Map<Type, Function>> conversions = new ConcurrentHashMap<>();
      Set<ModuleScope.Builder> imports = new HashSet<>(this.imports);
      Set<ModuleScope.Builder> exports = new HashSet<>(this.exports);
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

      return new ModuleScope.Builder(
          module,
          associatedType,
          polyglotSymbols,
          requestedTypes,
          methods,
          conversions,
          imports,
          exports);
    }

    public ModuleScope build() {
      if (moduleScope == null) {
        moduleScope =
            new ModuleScope(
                module,
                associatedType,
                polyglotSymbols,
                types,
                methods,
                conversions,
                imports,
                exports);
      }
      return moduleScope;
    }

    public void reset() {
      polyglotSymbols = new HashMap<>();
      // can't clear types because on recompilation methods etc will be assigned to the new one
      // types = new HashMap<>();
      methods = new ConcurrentHashMap<>();
      conversions = new ConcurrentHashMap<>();
      imports = new HashSet<>();
      exports = new HashSet<>();
      moduleScope = null;
    }
  }
}
