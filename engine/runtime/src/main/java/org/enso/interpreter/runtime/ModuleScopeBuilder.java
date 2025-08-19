package org.enso.interpreter.runtime;

import java.util.function.Supplier;
import org.enso.compiler.context.CompilerContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.ModuleScope;

/**
 * Module scope builder to allow runtime infrastructure to populate {@link ModuleScope} during
 * {@link Compiler} execution. This interface is needed by {@link Type} and {@link AtomConstructor}
 * and {@link Builtins} that modify the scope outside of this package. The {@code IrToTruffle} code
 * itself is using directly the package private implementation of {@link
 * TruffleCompilerModuleScopeBuilder}.
 */
public sealed interface ModuleScopeBuilder permits TruffleCompilerModuleScopeBuilder {
  /**
   * This scope builder has 1:1 association with {@link CompilerContext}'s module scope builder.
   *
   * @return the associated module scope builder
   */
  public CompilerContext.ModuleScopeBuilder toCompilerBuilder();

  /**
   * This scope builder is creating {@link ModuleScope} for a particular odule.
   *
   * @return the module this scope will become scope of
   */
  public Module getModule();

  /**
   * Each {@link #getModule} has one associated type. This method provides access to it bypassing
   * access to {@link ModuleScope#getAssociatedType()}.
   *
   * @return the associated type
   */
  public Type getAssociatedType();

  /**
   * Queries list of already registered types.
   *
   * @param name name to search for
   * @param ignoreAssociatedType ignore {@link #getAssociatedType()}
   * @return a type or {@code null}
   */
  public Type getType(String name, boolean ignoreAssociatedType);

  public Type registerType(Type type);

  public void registerMethod(Type tpeKey, String name, Supplier<Function> fun);

  public void registerAllMethodsOfTypeToScope(Type tpe, ModuleScopeBuilder scope);

  /** Finishes building of the scope */
  public void finish();

  /**
   * Access to scope this builder has built by {@link #finish}.
   *
   * @return ModuleScope, if the builder has already been `built`
   * @throws Error nasty error when this method is called sooner than {@link #finish}
   */
  public ModuleScope asModuleScope();
}
