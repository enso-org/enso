package org.enso.interpreter.runtime.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.enso.compiler.context.CompilerContext;
import org.enso.interpreter.runtime.EnsoContext;
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
  private final Map<String, Supplier<TruffleObject>> polyglotSymbols;
  private final Map<String, Type> types;
  private final Map<Type, Map<String, Supplier<Function>>> methods;

  /**
   * First key is target type, second key is source type. The value is the conversion function from
   * source to target.
   */
  private final Map<Type, Map<Type, Function>> conversions;

  private final Set<ImportExportScope> imports;
  private final Set<ImportExportScope> exports;

  private static final Type noTypeKey;

  static {
    noTypeKey = Type.noType();
  }

  public ModuleScope(
      Module module,
      Type associatedType,
      Map<String, Supplier<TruffleObject>> polyglotSymbols,
      Map<String, Type> types,
      Map<Type, Map<String, Supplier<Function>>> methods,
      Map<Type, Map<Type, Function>> conversions,
      Set<ImportExportScope> imports,
      Set<ImportExportScope> exports) {
    this.module = module;
    this.associatedType = associatedType;
    this.polyglotSymbols = polyglotSymbols;
    this.types = types;
    this.methods = methods;
    this.conversions = conversions;
    this.imports = imports;
    this.exports = exports;
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
    var definedWithAtom = type.getDefinitionScope().getMethodForType(type, name);
    if (definedWithAtom != null) {
      return definedWithAtom;
    }

    var definedHere = getMethodForType(type, name);
    if (definedHere != null) {
      return definedHere;
    }

    return imports.stream()
        .map(scope -> scope.getExportedMethod(type, name))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  /**
   * Looks up a conversion method from source type to target type. The conversion method
   * implementation looks like this:
   *
   * <pre>
   *   Target_Type.from (other : Source_Type) = ...
   * </pre>
   *
   * The conversion method is first looked up in the scope of the source type, then in the scope of
   * the target type and finally in all the imported scopes.
   *
   * @param source Source type
   * @param target Target type
   * @return The conversion method or null if not found.
   */
  @CompilerDirectives.TruffleBoundary
  public Function lookupConversionDefinition(Type source, Type target) {
    Function definedWithSource = source.getDefinitionScope().getConversionsFor(target).get(source);
    if (definedWithSource != null) {
      return definedWithSource;
    }
    Function definedWithTarget = target.getDefinitionScope().getConversionsFor(target).get(source);
    if (definedWithTarget != null) {
      return definedWithTarget;
    }
    Function definedHere = getConversionsFor(target).get(source);
    if (definedHere != null) {
      return definedHere;
    }
    return imports.stream()
        .map(scope -> scope.getExportedConversion(source, target))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  Function getExportedMethod(Type type, String name) {
    var here = getMethodForType(type, name);
    if (here != null) {
      return here;
    }
    return exports.stream()
        .map(scope -> scope.getMethodForType(type, name))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  Function getExportedConversion(Type type, Type target) {
    Function here = getConversionsFor(target).get(type);
    if (here != null) {
      return here;
    }
    return exports.stream()
        .map(scope -> scope.getConversionForType(target, type))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  public List<Type> getAllTypes(String name) {
    var tpes = new ArrayList<Type>(types.size() + 1);
    var tpe0 = getType(name, false);
    if (tpe0 != null) tpes.add(tpe0);
    tpes.addAll(types.values());
    return tpes;
  }

  public List<Type> getAllTypes() {
    return types.values().stream().collect(Collectors.toUnmodifiableList());
  }

  @ExportMessage.Ignore
  public Type getType(String name, boolean ignoreAssociatedType) {
    if (!ignoreAssociatedType && associatedType.getName().equals(name)) {
      return associatedType;
    }
    return types.get(name);
  }

  @ExportMessage.Ignore
  public boolean hasType(Type type) {
    return types.get(type.getName()) == type;
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

  Map<Type, Function> getConversionsFor(Type type) {
    var result = conversions.get(type);
    if (result == null) {
      return new LinkedHashMap<>();
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
   * Finds a polyglot symbol supplier. The supplier will then load the provided {@code symbolName}
   * when its {@link Supplier#get()} method is called.
   *
   * @param symbolName name of the symbol to search for
   * @return non-{@code null} supplier of a polyglot symbol imported into this scope
   */
  public Supplier<TruffleObject> getPolyglotSymbolSupplier(String symbolName) {
    var supplier = polyglotSymbols.get(symbolName);
    if (supplier != null) {
      return supplier;
    }
    var ctx = EnsoContext.get(null);
    var err = ctx.getBuiltins().error().makeMissingPolyglotImportError(symbolName);
    return CachingSupplier.forValue(err);
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
    return "Scope" + module;
  }

  public static class Builder {

    @CompilerDirectives.CompilationFinal private ModuleScope moduleScope = null;
    private final Module module;
    private final Type associatedType;
    private final Map<String, Supplier<TruffleObject>> polyglotSymbols;
    private final Map<String, Type> types;
    private final Map<Type, Map<String, Supplier<Function>>> methods;
    private final Map<Type, Map<Type, Function>> conversions;
    private final Set<ImportExportScope> imports;
    private final Set<ImportExportScope> exports;

    public Builder(Module module) {
      this.module = module;
      this.polyglotSymbols = new LinkedHashMap<>();
      this.types = new LinkedHashMap<>();
      this.methods = new LinkedHashMap<>();
      this.conversions = new LinkedHashMap<>();
      this.imports = new LinkedHashSet<>();
      this.exports = new LinkedHashSet<>();
      this.associatedType = Type.createSingleton(module.getName().item(), this, null, false, false);
    }

    public Builder(Module module, Map<String, Type> types) {
      this.module = module;
      this.polyglotSymbols = new LinkedHashMap<>();
      this.types = types;
      this.methods = new LinkedHashMap<>();
      this.conversions = new LinkedHashMap<>();
      this.imports = new LinkedHashSet<>();
      this.exports = new LinkedHashSet<>();
      this.associatedType = Type.createSingleton(module.getName().item(), this, null, false, false);
    }

    public Builder(
        Module module,
        Type associatedType,
        Map<String, Supplier<TruffleObject>> polyglotSymbols,
        Map<String, Type> types,
        Map<Type, Map<String, Supplier<Function>>> methods,
        Map<Type, Map<Type, Function>> conversions,
        Set<ImportExportScope> imports,
        Set<ImportExportScope> exports) {
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
      return methods.computeIfAbsent(tpeKey, k -> new LinkedHashMap<>());
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
        methodMap.put(method, CachingSupplier.forValue(function));
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
      assert moduleScope == null;
      Map<String, Supplier<Function>> methodMap = ensureMethodMapFor(type);

      // Builtin types will have double definition because of
      // BuiltinMethod and that's OK
      if (methodMap.containsKey(method) && !type.isBuiltin()) {
        throw new RedefinedMethodException(type.getName(), method);
      } else {
        methodMap.put(method, CachingSupplier.wrap(supply));
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
      var sourceMap = conversions.computeIfAbsent(toType, k -> new LinkedHashMap<>());
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
     * @param symbolFactory the value being exposed
     */
    public void registerPolyglotSymbol(String name, Supplier<TruffleObject> symbolFactory) {
      assert moduleScope == null;
      polyglotSymbols.put(name, CachingSupplier.wrap(symbolFactory));
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
    public void addImport(ImportExportScope scope) {
      assert moduleScope == null;
      imports.add(scope);
    }

    /**
     * Adds an information about the module exporting another module.
     *
     * @param scope the exported scope
     */
    public void addExport(ImportExportScope scope) {
      assert moduleScope == null;
      exports.add(scope);
    }

    public Module getModule() {
      return module;
    }

    /**
     * Create a new ModuleScope.Builder which inherits from `this` `module` and `types` that need to
     * survive the compilation.
     *
     * @return new ModuleScope.Builder
     */
    public Builder newBuilderInheritingTypes() {
      return new Builder(this.module, new LinkedHashMap<>(this.types));
    }

    /**
     * Materializes the builder and ensures that no further modifications to ModuleScope are
     * possible. Action is idempotent.
     *
     * @return an immutable ModuleScope
     */
    public ModuleScope build() {
      if (moduleScope == null) {
        moduleScope =
            new ModuleScope(
                module,
                associatedType,
                Collections.unmodifiableMap(polyglotSymbols),
                Collections.unmodifiableMap(types),
                Collections.unmodifiableMap(methods),
                Collections.unmodifiableMap(conversions),
                Collections.unmodifiableSet(imports),
                Collections.unmodifiableSet(exports));
      }
      return moduleScope;
    }

    public static ModuleScope.Builder fromCompilerModuleScopeBuilder(
        CompilerContext.ModuleScopeBuilder scopeBuilder) {
      return ((TruffleCompilerModuleScopeBuilder) scopeBuilder).unsafeScopeBuilder();
    }

    /**
     * Return a view on `this` as a ModuleScope, rather than its builder.
     *
     * @return ModuleScope, if the builder has already been `built`, a proxy instance with the
     *     currently registered entities
     */
    public ModuleScope asModuleScope() {
      if (moduleScope != null) {
        return moduleScope;
      } else {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        return createModuleScope();
      }
    }

    @CompilerDirectives.TruffleBoundary
    private ModuleScope createModuleScope() {
      return new ModuleScope(
          module,
          associatedType,
          Collections.unmodifiableMap(polyglotSymbols),
          Collections.unmodifiableMap(types),
          Collections.unmodifiableMap(methods),
          Collections.unmodifiableMap(conversions),
          Collections.unmodifiableSet(imports),
          Collections.unmodifiableSet(exports));
    }

    @Override
    public java.lang.String toString() {
      StringBuilder builder = new StringBuilder();
      builder.append("ModuleScope builder for " + module.getName());
      builder.append(",\n");
      builder.append("Polyglot Symbols: " + polyglotSymbols);
      builder.append(",\n");
      builder.append("Methods: " + methods);
      builder.append(",\n");
      builder.append("Conversions: " + conversions);
      builder.append(",\n");
      builder.append("Imports: " + imports);
      builder.append(",\n");
      builder.append("Exports: " + exports);
      builder.append(",\n");
      builder.append("Types: " + types);
      return builder.toString();
    }
  }
}
