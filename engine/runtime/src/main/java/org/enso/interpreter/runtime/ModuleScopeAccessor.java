package org.enso.interpreter.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.ImportExportScope;
import org.enso.interpreter.runtime.scope.ModuleScope;

/**
 * Accessor between this package and package that defines {@link ModuleScopeBuilder}. Controls who
 * can start building new scope:
 *
 * <ul>
 *   <li>either implementation of {@link CompilerContext}
 *   <li>or creators of a module - like in {@link Module#newModuleWith}
 * </ul>
 */
public abstract class ModuleScopeAccessor {
  private static ModuleScopeAccessor INSTANCE;

  static {
    var forceInitialization = ModuleScope.class;
    try {
      Class.forName(forceInitialization.getName(), true, forceInitialization.getClassLoader());
    } catch (ClassNotFoundException ex) {
      throw new IllegalStateException(ex);
    }
  }

  static ModuleScopeAccessor getInstance() {
    assert INSTANCE != null;
    return INSTANCE;
  }

  /** Registers the only one implementation of this accessor. */
  protected ModuleScopeAccessor() {
    assert INSTANCE == null : "Allow only one implementation";
    INSTANCE = this;
  }

  // protected abstract ModuleScopeBuilder newScopeBuilder(Module m, Consumer<ModuleScope> update);
  protected abstract Function findMethodForType(
      Type tpe, final Map<Type, Map<String, Supplier<Function>>> m, String name);

  protected abstract Supplier<TruffleObject> findPolyglotSymbolSupplier(
      Map<String, Supplier<TruffleObject>> ps, String symbolName);

  protected abstract ModuleScope newModuleScope(
      Module module,
      Type associatedType,
      Map<String, Supplier<TruffleObject>> unmodifiableMap,
      Map<String, Type> unmodifiableMap0,
      Map<Type, Map<String, Supplier<Function>>> unmodifiableMap1,
      Map<Type, Map<Type, Supplier<Function>>> unmodifiableMap2,
      Set<ImportExportScope> unmodifiableSet,
      Set<ImportExportScope> unmodifiableSet0);
}
