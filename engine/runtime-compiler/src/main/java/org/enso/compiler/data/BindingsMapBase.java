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
 * Represents immutable (as much as possible) view of a "binding map". A utility structure for
 * resolving symbols in a given module.
 */
abstract class BindingsMapBase implements IRPass.IRMetadata {
  /** either {@link State} or {@code Supplier<State>} */
  private Object state;

  BindingsMapBase() {}

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
    return switch (this.state) {
      case State s -> s;
      case Supplier<?> supply -> {
        var s = (State) supply.get();
        assert s != null;
        this.state = s;
        yield s;
      }
      default -> throw new IllegalStateException();
    };
  }

  /**
   * Modifies the state of the "bindings map". This is the only way to mutate the state to a
   * concrete value.
   *
   * @param newState new state to use since now
   * @see #setLazyState
   */
  final void setState(State newState) {
    this.state = newState;
  }

  /**
   * Modifies the state of the "bindings map". This is the only way to mutate the state to a
   * "supplier" of the state.
   *
   * @param newState new state to use since now
   * @see #setState
   */
  final void setLazyState(Supplier<State> futureState) {
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
