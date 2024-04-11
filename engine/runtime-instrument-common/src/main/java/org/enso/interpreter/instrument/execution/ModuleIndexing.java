package org.enso.interpreter.instrument.execution;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicBoolean;
import org.enso.compiler.core.IR;
import org.enso.interpreter.runtime.Module;

public final class ModuleIndexing {

  /**
   * State of indexing encapsulating for a given IR
   *
   * @param isIndexed true, if IR has been already indexed. False otherwise
   * @param ir IR of a module that has been/needs to be indexed
   */
  public record IndexState(boolean isIndexed, IR ir) {
    private IndexState toIndexed() {
      assert !isIndexed;
      return new IndexState(true, ir);
    }

    private IndexState withIr(IR ir) {
      assert isIndexed;
      return new IndexState(true, ir);
    }
  }

  private final ConcurrentMap<Module, IndexState> modules;

  private ModuleIndexing() {
    this.modules = new ConcurrentHashMap<>();
  }

  public static ModuleIndexing createInstance() {
    return new ModuleIndexing();
  }

  /**
   * @return true, if module has been isIndexed. False otherwise.
   */
  public IndexState find(Module module) {
    return modules.get(module);
  }

  /**
   * Get index state for a module or assigns a new one.
   *
   * @param module module for which lookup is performed
   * @param ir IR for which index is calculated, if new
   * @return index state assigned to the module, or a new one if absent
   */
  public IndexState getOrCreateFresh(Module module, IR ir) {
    return modules.computeIfAbsent(module, m -> new IndexState(false, ir));
  }

  /**
   * Attempts to update the index state for a module. If the provided state does not match the one
   * currently assigned to the module, no update is performed.
   *
   * @param state reference index state to be updated
   * @return true if the operation of updating the state was successful, false if the reference
   *     state was not up-to-date.
   */
  public boolean markAsIndexed(Module module, IndexState state) {
    AtomicBoolean updated = new AtomicBoolean(false);
    modules.compute(
        module,
        (k, v) -> {
          if (v == state) {
            updated.set(true);
            return state.toIndexed();
          } else {
            return v;
          }
        });
    return updated.get();
  }

  /**
   * Attempts to update the index state for a module with a given IR. If the provided state does not
   * match the one currently assigned to the module, no update is performed.
   *
   * @param state reference index state to be updated
   * @param ir IR for which the index has been calculated
   * @return true if the operation of updating the state was successful, false if the reference
   *     state was not up-to-date.
   */
  public boolean updateState(Module module, IndexState state, IR ir) {
    AtomicBoolean updated = new AtomicBoolean(false);
    modules.compute(
        module,
        (k, v) -> {
          if (v == state) {
            updated.set(true);
            return state.withIr(ir);
          } else {
            return v;
          }
        });
    return updated.get();
  }

  /** Clear index state for a provided module. */
  public void markIndexAsDirty(Module module) {
    modules.compute(module, (k, v) -> null);
  }
}
