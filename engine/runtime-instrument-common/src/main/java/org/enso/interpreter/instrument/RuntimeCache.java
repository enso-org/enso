package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.function.Supplier;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.service.ExecutionService;
import org.enso.interpreter.service.GuestExecutionService;

/** A storage for computed values. */
public final class RuntimeCache implements java.util.function.Function<String, Object> {
  private final Map<UUID, Observable> cache = new ConcurrentHashMap<>();
  private final Map<UUID, TypeInfo> types = new HashMap<>();
  private final Map<UUID, ExecutionService.FunctionCallInfo> calls = new HashMap<>();
  private Consumer<UUID> observer;
  private final GuestExecutionService executionService;

  public RuntimeCache(GuestExecutionService executionService) {
    this.executionService = executionService;
  }

  /**
   * Add value to the cache if it is possible. If any observer registered for `key` updates, it will be notified.
   * DataflowErrors are never cached.
   *
   * @param key the key of an entry.
   * @param value the added value.
   * @return {@code true} if the value was added to the cache.
   */
  @CompilerDirectives.TruffleBoundary
  public boolean offer(UUID key, Object value) {
    var observable = cache.get(key);
    // If one `offers` the value, then it means an Observable has been assigned to the key
    assert observable != null;
    var notDataflowError = !(value instanceof DataflowError);
    observable.update(value, notDataflowError, executionService);
    return notDataflowError;
  }

  /** Get the observable from the cache. */
  public Observable get(UUID key) {
    return cache.computeIfAbsent(key, k -> new Observable(key));
  }

  /** Get the observable from the cache. */
  public Observable get(UUID expressionId, UUID downstreamDependency) {
    var o = cache.computeIfAbsent(expressionId, _ -> new Observable(expressionId));
    return downstreamDependency == null ? o : o.register(cache.get(downstreamDependency));
  }

  public CompletionStage<Boolean> registerAction(UUID expressionId, ObservableAction action) {
    return cache
        .computeIfAbsent(expressionId, k -> new Observable(expressionId))
        .registerAction(action, executionService);
  }

  public boolean deregisterAction(UUID expressionId, UUID visualizationId) {
    var observable = cache.get(expressionId);
    if (observable != null) {
      return observable.deregisterAction(visualizationId);
    } else {
      return false;
    }
  }

  // Accessed in InstrumentorBuiltin
  @Override
  public Object apply(String uuid) {
    Object res;
    try {
      var key = UUID.fromString(uuid);
      var observable = cache.get(key);
      res = observable == null ? null : observable.get();
      var callback = observer;
      if (callback != null) {
        callback.accept(key);
      }
    } catch (IllegalArgumentException ex) {
      res = null;
    }
    return res;
  }

  /** Remove the value from the cache. */
  public Object remove(UUID key) {
    var ref = cache.remove(key);
    return ref == null ? null : ref.get();
  }

  /**
   * @return all cache keys.
   */
  public Set<UUID> getKeys() {
    return cache.keySet();
  }

  /** Clear the cached values. */
  public void clear() {
    cache.clear();
  }

  /**
   * Clear cached values of the provided kind.
   *
   * @param kind the kind of cached value to clear
   * @return the set of cleared keys
   */
  /*public Set<UUID> clear(CachePreferences.Kind kind) {
    var keys = preferences.get(kind);
    for (var key : keys) {
      cache.remove(key);
    }
    return keys;
  }*/

  /**
   * Cache the type of expression.
   *
   * @return the previously cached type.
   */
  @CompilerDirectives.TruffleBoundary
  public TypeInfo putType(UUID key, TypeInfo typeInfo) {
    return types.put(key, typeInfo);
  }

  /**
   * @return the cached type of the expression
   */
  @CompilerDirectives.TruffleBoundary
  public TypeInfo getType(UUID key) {
    return types.get(key);
  }

  /**
   * Cache the function call
   *
   * @param key the expression associated with the function call.
   * @param call the function call.
   * @return the function call that was previously associated with this expression.
   */
  @CompilerDirectives.TruffleBoundary
  public ExecutionService.FunctionCallInfo putCall(
      UUID key, ExecutionService.FunctionCallInfo call) {
    if (call == null) {
      return calls.remove(key);
    }
    return calls.put(key, call);
  }

  /**
   * @return the cached function call associated with the expression.
   */
  @CompilerDirectives.TruffleBoundary
  public ExecutionService.FunctionCallInfo getCall(UUID key) {
    return calls.get(key);
  }

  /**
   * @return the cached method calls.
   */
  public Set<UUID> getCalls() {
    return calls.keySet();
  }

  /**
   * Remove the function call from the cache.
   *
   * @param key the expression associated with the function call.
   */
  public void removeCall(UUID key) {
    calls.remove(key);
  }

  /** Clear the cached calls. */
  public void clearCalls() {
    calls.clear();
  }

  /** Remove the type associated with the provided key. */
  public void removeType(UUID key) {
    types.remove(key);
  }

  /** Clear the cached types. */
  public void clearTypes() {
    types.clear();
  }

  /**
   * Executes a query while tracking access to the cache by {@code callback} observer.
   *
   * @param callback call with accessed UUIDs
   * @param scope the code to execute
   * @return value computed by the {@code scope}
   * @param <V> type of the returned value
   */
  public <V> V runQuery(Consumer<UUID> callback, Supplier<V> scope) {
    var previousCallback = this.observer;
    this.observer = callback;
    try {
      return scope.get();
    } finally {
      this.observer = previousCallback;
    }
  }
}
