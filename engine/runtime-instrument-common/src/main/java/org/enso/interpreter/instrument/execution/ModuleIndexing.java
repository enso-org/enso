package org.enso.interpreter.instrument.execution;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import org.enso.compiler.core.IR;
import org.enso.interpreter.runtime.Module;

public final class ModuleIndexing {

  /**
   * State of indexing encapsulating the indexed IR
   *
   * @param isIndexed true, if IR has been already indexed. False otherwise
   * @param ir IR of a module that has been/needs to be indexed
   * @param hasSource true, if
   */
  public record IndexState(boolean isIndexed, IR ir, IndexState from) {
    private IndexState toIndexed() {
      assert !isIndexed;
      assert from == null;
      return new IndexState(true, ir, this);
    }

    private IndexState withIr(IR ir) {
      assert isIndexed;
      return new IndexState(true, ir, this);
    }
  }

  private final ConcurrentMap<Module, IndexState> modules;

  private ModuleIndexing() {
    this.modules = new ConcurrentHashMap<>();
  }

  public static ModuleIndexing getInstance() {
    return new ModuleIndexing();
  }

  /**
   * @return true, if module has been isIndexed. False otherwise.
   */
  public boolean isIndexed(Module module) {
    var result = modules.get(module);
    return result != null && result.isIndexed();
  }

  public IndexState getOrCreateFresh(Module module, IR ir) {
    return modules.computeIfAbsent(module, m -> new IndexState(false, ir, null));
  }

  /**
   * Stores the updated state for the module iff the state was the last to be returned.
   *
   * @return true if the operation of updating the state was successful, false otherwise.
   */
  public boolean markAsIndexed(Module module, IndexState state) {
    var computed = modules.compute(module, (k, v) -> v == state ? state.toIndexed() : v);
    return computed.from() == state;
  }

  public boolean updateState(Module module, IndexState state, IR ir) {
    var computed = modules.compute(module, (k, v) -> v == state ? state.withIr(ir) : v);
    return computed.from() == state;
  }

  /** Marks the module as requiring indexing. */
  public void markIndexAsDirty(Module module) {
    modules.compute(module, (k, v) -> null);
  }
}
