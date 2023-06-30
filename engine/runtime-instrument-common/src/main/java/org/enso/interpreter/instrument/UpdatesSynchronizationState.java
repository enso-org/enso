package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

/**
 * The synchronization state of runtime updates.
 *
 * <p>The thread executing the program can be interrupted at any moment. For example, the interrupt
 * may happen when an expression is computed and the runtime state is changed, but before the update
 * is sent to the user. And since the runtime state has changed, the server won't send the updates
 * during the next execution. This class is supposed to address this issue keeping in sync the
 * runtime state and the update messages.
 *
 * <h1>Implementation
 *
 * <p>When implementing the synchronization, keep in mind the following principles:
 *
 * <ul>
 *   <li>Remove the synchronization flag before changing the server state.
 *   <li>Set the synchronization flag after the update is sent to the user.
 * </ul>
 *
 * This way the server is guaranteed to send the update message at least once, regardless of when
 * the thread interrupt has occurred.
 *
 * <p>The state consists of the following components:
 *
 * <ul>
 *   <li>Expressions state. Tracks all message updates that are sent when the expression metadata
 *       (e.g. the type or the underlying method pointer) is changed.
 *   <li>Method pointers state. Tracks message updates containing method pointers. Messages with
 *       method pointers are tracked separately from the expressions state because they have
 *       different invalidation rules. E.g., they should always be re-sent when the execution item
 *       is popped from the stack.
 *   <li>Visualizations state. Tracks the state of visualization updates.
 * </ul>
 */
public class UpdatesSynchronizationState {

  private final Set<UUID> expressionsState = new HashSet<>();
  private final Set<UUID> visualizationsState = new HashSet<>();
  private final Set<UUID> methodPointersState = new HashSet<>();

  @Override
  public String toString() {
    return "UpdatesSynchronizationState{"
        + "expressionsState="
        + expressionsState
        + ", visualizationsState="
        + visualizationsState
        + ", methodPointersState="
        + methodPointersState
        + '}';
  }

  /**
   * Invalidate the state of the given expression.
   *
   * @param key the expression id.
   */
  public void invalidate(UUID key) {
    synchronized (this) {
      expressionsState.remove(key);
      visualizationsState.remove(key);
      methodPointersState.remove(key);
    }
  }

  /* Expressions */

  /**
   * Checks if the given expression update is synchronized.
   *
   * @param key the expression id.
   * @return {@code true} if the expression update is synchronized.
   */
  public boolean isExpressionSync(UUID key) {
    synchronized (expressionsState) {
      return expressionsState.contains(key);
    }
  }

  /**
   * Marks the given expression update as unsynchronized.
   *
   * @param key the expression id.
   */
  @CompilerDirectives.TruffleBoundary
  public void setExpressionUnsync(UUID key) {
    synchronized (expressionsState) {
      expressionsState.remove(key);
    }
  }

  /**
   * Marks the given expression update as synchronized.
   *
   * @param key the expression id.
   */
  public void setExpressionSync(UUID key) {
    synchronized (expressionsState) {
      expressionsState.add(key);
    }
  }

  /* Visualizations */

  /**
   * Checks if the given visualization update is synchronized.
   *
   * @param key the expression id.
   * @return {@code true} if the visualization update is synchronized.
   */
  public boolean isVisualizationSync(UUID key) {
    synchronized (visualizationsState) {
      return visualizationsState.contains(key);
    }
  }

  /**
   * Marks the given visualization update as unsynchronized.
   *
   * @param key the expression id.
   */
  @CompilerDirectives.TruffleBoundary
  public void setVisualizationUnsync(UUID key) {
    synchronized (visualizationsState) {
      visualizationsState.remove(key);
    }
  }

  /**
   * Marks the given visualization update as synchronized.
   *
   * @param key the expression id.
   */
  public void setVisualizationSync(UUID key) {
    synchronized (visualizationsState) {
      visualizationsState.add(key);
    }
  }

  /* Method pointers */

  /**
   * Checks if the given method pointer is synchronized.
   *
   * @param key the expression id.
   * @return {@code true} if the method pointer update is synchronized.
   */
  public boolean isMethodPointerSync(UUID key) {
    synchronized (methodPointersState) {
      return methodPointersState.contains(key);
    }
  }

  /**
   * Marks the method pointer as synchronized.
   *
   * @param key the expression id.
   */
  public void setMethodPointerSync(UUID key) {
    synchronized (methodPointersState) {
      methodPointersState.add(key);
    }
  }

  /** Clears the synchronization state of all method pointers. */
  public void clearMethodPointersState() {
    synchronized (methodPointersState) {
      methodPointersState.clear();
    }
  }
}
