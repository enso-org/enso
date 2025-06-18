package org.enso.compiler.data;

import static org.enso.scala.wrapper.ScalaConversions.nil;

import java.util.function.Supplier;
import org.enso.compiler.data.BindingsMap.DefinedEntity;
import org.enso.compiler.data.BindingsMap.ModuleReference;
import org.enso.compiler.data.BindingsMap.ResolvedImport;
import org.enso.compiler.data.BindingsMap.ResolvedName;
import org.enso.compiler.pass.IRPass;
import scala.collection.immutable.List;
import scala.collection.immutable.Map;
import scala.collection.immutable.Map$;

/**
 * A utility structure for resolving symbols in a given module. Represents immutable (as much as
 * possible) view of a "binding map". The {@code state} can mutate, but it is guaranteed to be
 * changed atomically to ensure consistency - all elements of the state are always changed at once.
 */
abstract class BindingsMapBase implements IRPass.IRMetadata {
  /**
   * @GuardedBy("this"). Either {@link State} or {@code Supplier<State>}
   */
  private Object state;

  BindingsMapBase(State initial) {
    this.state = initial;
  }

  public final List<DefinedEntity> definedEntities() {
    return getState().definedEntities();
  }

  public final ModuleReference currentModule() {
    return getState().currentModule();
  }

  public final List<ResolvedImport> resolvedImports() {
    return getState().resolvedImports();
  }

  public final Map<String, List<ResolvedName>> exportedSymbols() {
    return getState().exportedSymbols();
  }

  //
  // Non-public implementation for a subclass
  //

  final State getState() {
    AGAIN:
    for (; ; ) {
      Object tmp;
      synchronized (this) {
        tmp = this.state;
      }
      var currentState =
          switch (tmp) {
            case State s -> s;
            case Supplier<?> supply -> {
              var s = (State) supply.get();
              assert s != null;
              synchronized (this) {
                if (this.state != tmp) {
                  // try again
                  yield null;
                } else {
                  this.state = s;
                  yield s;
                }
              }
            }
            default -> throw new IllegalStateException();
          };
      if (currentState != null) {
        return currentState;
      }
    }
  }

  /**
   * Modifies the state of the "bindings map". This is the only way to mutate the state to a
   * concrete value.
   *
   * @param originalState the previous state we want to update
   * @param newState new state to use since now
   * @see #setLazyState
   */
  final synchronized void setState(State newState) {
    this.state = newState;
  }

  /**
   * Modifies the state of the "bindings map". This is the only way to mutate the state to a
   * "supplier" of the state.
   *
   * @param originalState the previous state we want to update
   * @param newState new state to use since now
   * @see #setState
   */
  final synchronized void setLazyState(Supplier<State> futureState) {
    this.state = futureState;
  }

  /** Immutable state of a binding map. */
  static record State(
      List<DefinedEntity> definedEntities,
      ModuleReference currentModule,
      List<ResolvedImport> resolvedImports,
      Map<String, List<ResolvedName>> exportedSymbols) {
    State(List<DefinedEntity> definedEntities, ModuleReference currentModule) {
      this(definedEntities, currentModule, nil(), Map$.MODULE$.empty());
    }

    final State withResolvedImports(List<ResolvedImport> newImports) {
      return new State(definedEntities, currentModule, newImports, exportedSymbols);
    }

    final State withCurrentModule(ModuleReference newModule) {
      return new State(definedEntities, newModule, resolvedImports, exportedSymbols);
    }

    final State withExportedSymbols(Map<String, List<ResolvedName>> newSymbols) {
      return new State(definedEntities, currentModule, resolvedImports, newSymbols);
    }
  }
}
