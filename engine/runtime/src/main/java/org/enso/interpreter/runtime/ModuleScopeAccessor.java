package org.enso.interpreter.runtime;

import org.enso.interpreter.runtime.scope.ModuleScopeBuilder;

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
  private static ModuleScopeAccessor INIT;
  static final ModuleScopeAccessor INSTANCE;

  static {
    var forceInitialization = ModuleScopeBuilder.class;
    try {
      Class.forName(forceInitialization.getName(), true, forceInitialization.getClassLoader());
    } catch (ClassNotFoundException ex) {
      throw new IllegalStateException(ex);
    }
    assert INIT != null : "Class " + forceInitialization + " should register accessor";
    INSTANCE = INIT;
    INIT = null;
  }

  /** Registers the only one implementation of this accessor. */
  protected ModuleScopeAccessor() {
    assert INSTANCE == null : "Allow only one implementation";
    INIT = this;
  }

  protected abstract ModuleScopeBuilder newScopeBuilder(Module m);
}
