package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CompilerDirectives;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.service.ExecutionService;
import org.enso.interpreter.service.GuestExecutionService;
import org.enso.polyglot.RuntimeID;

/** A storage for computed values. */
public final class RuntimeCache implements java.util.function.Function<String, Object> {
  private static int COUNTER = 0;
  public final int id;
  private final Map<UUID, Observable> cache = new ConcurrentHashMap<>();
  private final Map<UUID, TypeInfo> types = new HashMap<>();
  private final Map<UUID, ExecutionService.FunctionCallInfo> calls = new HashMap<>();
  private Consumer<UUID> observer;
  private final GuestExecutionService executionService;
  private final Map<UUID, FunctionCallInstrumentationNode.FunctionCall> enterables =
      new HashMap<>();
  private RuntimeID localCallUUID;

  public RuntimeCache(GuestExecutionService executionService) {
    id = COUNTER;
    COUNTER = COUNTER + 1;
    this.executionService = executionService;
  }

  public RuntimeID getLocalCallUUID() {
    return localCallUUID;
  }

  public void invalidate(Set<RuntimeID> keys) {
    keys.stream().forEach(k -> enterables.remove(k.uuid()));
  }

  public FunctionCallInstrumentationNode.FunctionCall enterable(UUID key) {
    return enterables.get(key);
  }

  public void updateEnterable(UUID key, FunctionCallInstrumentationNode.FunctionCall call) {
    enterables.put(key, call);
  }

  /**
   * Add value to the cache if it is possible. If any observer registered for `key` updates, it will
   * be notified. DataflowErrors are never cached.
   *
   * @param key the key of an entry.
   * @param value the added value.
   * @return {@code true} if the value was added to the cache.
   */
  @CompilerDirectives.TruffleBoundary
  public boolean offer(RuntimeID key, Object value) {
    var observable = cache.get(key.uuid());
    assert observable != null;
    if (observable.isExternal()) {
      return observable.update(value, executionService);
    } else {
      return false;
    }
  }

  @CompilerDirectives.TruffleBoundary
  public void notify(RuntimeID key, Object value) {
    var observable = cache.get(key.uuid());
    assert observable != null;
    if (observable.isExternal()) {
      observable.notify(value, executionService);
    }
  }

  public Observable get(UUID key) {
    return cache.get(key);
  }

  /** Gets an instance of Observable from the cache. */
  public Observable get(RuntimeID key) {
    return cache.computeIfAbsent(key.uuid(), k -> Observable.fromUUID(key));
  }

  /** Get the observable from the cache. */
  public Observable get(RuntimeID expressionId, RuntimeID downstreamDependency) {
    var o = cache.computeIfAbsent(expressionId.uuid(), _ -> Observable.fromUUID(expressionId));
    return downstreamDependency == null ? o : o.register(cache.get(downstreamDependency));
  }

  /**
   * Infer downstream dependencies of an {@code Observable} identified by the given id.
   *
   * @param id ID of the dependency to look for
   * @return a list of Observables having {@code id} as an upstream dependency
   */
  public List<Observable> downstreamOf(RuntimeID id) {
    var downstream = new LinkedList<Observable>();
    for (Observable o : cache.values()) {
      if (o.hasDependency(id)) {
        downstream.add(o);
      }
    }
    return downstream;
  }

  public CompletionStage<Boolean> registerAction(
      RuntimeID expressionId, ObservableVisualization action) {
    return cache
        .computeIfAbsent(expressionId.uuid(), k -> new ExternalObservable(expressionId, true))
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

  public Set<Observable> allCached() {
    return cache.values().stream()
        .filter(v -> v instanceof ExternalObservable)
        .collect(Collectors.toSet());
  }

  public void setEntryNode(RuntimeID id) {
    localCallUUID = id;
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

  @Override
  public String toString() {
    return "RuntimeCache[id=" + id + "]";
  }
}
