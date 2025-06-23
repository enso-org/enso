package org.enso.compiler.data;

import static org.enso.scala.wrapper.ScalaConversions.nil;

import java.util.function.Function;
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
   * @GuardedBy("this"). Accessed in {@link #getState} and {@link #updateState} methods. Value can
   * be either {@link State} or {@code Supplier<State>}
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

  /**
   * Obtain when a consistent state of the map is needed. All values in the state are known to have
   * been consistent at some point of time. Never hold the state for too long. Always obtain a fresh
   * one.
   *
   * @return consistent state of the "binding map" as of "now"
   */
  final State getState() {
    while (true) {
      Supplier<?> tmp;
      synchronized (this) {
        if (this.state instanceof State s) {
          return s;
        } else {
          tmp = (Supplier<?>) this.state;
        }
      }
      var s = (State) tmp.get();
      assert s != null;
      synchronized (this) {
        if (this.state == tmp) {
          this.state = s;
        }
      }
    }
  }

  /**
   * Modifies the state of the "bindings map". This is the only way to mutate the state to a
   * different value.
   *
   * @param updator function that takes current version of state and updates it to new version, the
   *     function may be invoke multiple times when there are concurrent requests to update the
   *     state
   * @param lazy delay the update until the state is requested
   */
  final void updateState(Function<State, State> updator, boolean lazy) {
    while (true) {
      var currentState = this.getState();
      if (lazy) {
        synchronized (this) {
          if (this.state == currentState) {
            Supplier<State> fn = () -> updator.apply(currentState);
            this.state = fn;
            return;
          }
        }
      } else {
        var newState = updator.apply(currentState);
        synchronized (this) {
          if (this.state == currentState) {
            this.state = newState;
            return;
          }
        }
      }
    }
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
