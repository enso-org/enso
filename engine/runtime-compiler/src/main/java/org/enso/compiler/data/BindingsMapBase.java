package org.enso.compiler.data;

import static org.enso.scala.wrapper.ScalaConversions.nil;

import org.enso.compiler.PackageRepository;
import org.enso.compiler.data.BindingsMap.DefinedEntity;
import org.enso.compiler.data.BindingsMap.ModuleReference;
import org.enso.compiler.data.BindingsMap.ResolvedImport;
import org.enso.compiler.data.BindingsMap.ResolvedName;
import org.enso.compiler.pass.IRPass;
import scala.collection.immutable.List;
import scala.collection.immutable.Map;

abstract class BindingsMapBase implements IRPass.IRMetadata {
  private State state;

  protected final State getState() {
    return this.state;
  }

  protected final void setState(State newState) {
    this.state = newState;
  }

  /** Immutable state of a binding map. */
  static record State(
      List<DefinedEntity> definedEntities,
      ModuleReference currentModule,
      List<ResolvedImport> resolvedImports,
      PackageRepository pendingRepository,
      Map<String, List<ResolvedName>> exportedSymbols) {
    State(List<DefinedEntity> definedEntities, ModuleReference currentModule) {
      this(definedEntities, currentModule, nil(), null, null);
    }

    /**
     * Other modules, imported by [[currentModule]]. private var _currentModule: ModuleReference
     * private var _resolvedImports: List[ResolvedImport] = List()
     */
    /**
     * Set to non-null after deserialization to signal that conversion to concrete values is needed
     * private var pendingRepository: PackageRepository = null
     */

    /**
     * Symbols exported by [[currentModule]]. private var _exportedSymbols: Map[String,
     * List[ResolvedName]] = Map()
     */
    final State withResolvedImports(List<ResolvedImport> newImports) {
      return new State(
          definedEntities, currentModule, newImports, pendingRepository, exportedSymbols);
    }

    final State withCurrentModule(ModuleReference newModule) {
      return new State(
          definedEntities, newModule, resolvedImports, pendingRepository, exportedSymbols);
    }

    final State withPendingRepository(PackageRepository newRepo) {
      return new State(definedEntities, currentModule, resolvedImports, newRepo, exportedSymbols);
    }

    final State withExportedSymbols(Map<String, List<ResolvedName>> newSymbols) {
      return new State(
          definedEntities, currentModule, resolvedImports, pendingRepository, newSymbols);
    }
  }
}
