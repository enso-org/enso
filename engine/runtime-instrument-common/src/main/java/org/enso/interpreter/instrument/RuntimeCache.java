package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CompilerDirectives;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import org.enso.common.CachePreferences;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.service.ExecutionService;
import org.enso.polyglot.ExternalUUID;
import org.enso.polyglot.RuntimeID;

/** A storage for computed values. */
public final class RuntimeCache implements Function<String, Object> {
  private static int ID_COUNTER = 0;

  private final int id;
  private final Map<RuntimeID, Reference<Object>> cache = new HashMap<>();
  private final Map<UUID, Reference<Object>> expressions = new HashMap<>();
  private final Map<UUID, TypeInfo> types = new HashMap<>();
  private final Map<UUID, ExecutionService.FunctionCallInfo> calls = new HashMap<>();
  private CachePreferences preferences = CachePreferences.empty();
  private Consumer<UUID> observer;
  private final Map<UUID, FunctionCallInstrumentationNode.FunctionCall> enterables =
      new HashMap<>();

  public RuntimeCache() {
    id = ID_COUNTER++;
  }

  /**
   * Add value to the cache if it is possible.
   *
   * @param key the key of an entry.
   * @param value the added value.
   * @return {@code true} if the value was added to the cache.
   */
  @CompilerDirectives.TruffleBoundary
  public CachedResult offer(RuntimeID key, Object value) {
    expressions.put(key.uuid(), new WeakReference<>(value));
    if (key.canBeCached()) {
      if (cache.containsKey(key)) {
        return new CachedResult(true, true);
      }
      var ref = new SoftReference<>(value);
      cache.put(key, ref);
      return new CachedResult(true, false);
    }
    return CachedResult.uncacheable();
  }

  /**
   * Encapsulates the state of encapsulating some value.
   *
   * @param canBeCached true if the value with the given runtime ID can be cached, false otherwise
   * @param valueAlreadyCached true if there exists already an cache entry for the given runtime ID,
   *     false otherwise
   */
  public record CachedResult(boolean canBeCached, boolean valueAlreadyCached) {
    public boolean updated() {
      return canBeCached && !valueAlreadyCached;
    }

    public static CachedResult uncacheable() {
      return new CachedResult(false, false);
    }
  }

  // TODO: avoid creating a temporary ID
  public Object get(UUID key) {
    return get(new ExternalUUID(key, false));
  }

  /** Get the value from the cache. */
  public Object get(RuntimeID key) {
    var ref = cache.get(key);
    return ref != null ? ref.get() : null;
  }

  // TODO: avoid creating a temporary ID
  public boolean hasValue(UUID key) {
    return cache.containsKey(ExternalUUID.create(key));
  }

  /** Get the value from the cache. */
  public Object getAnyValue(UUID key) {
    var ref = expressions.get(key);
    return ref != null ? ref.get() : null;
  }

  // Accessed in InstrumentorBuiltin
  @Override
  public Object apply(String uuid) {
    Object res;
    try {
      var key = UUID.fromString(uuid);
      var ref = expressions.get(key);
      res = ref != null ? ref.get() : null;
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
  public Object remove(RuntimeID key) {
    var ref = cache.remove(key);
    return ref == null ? null : ref.get();
  }

  /**
   * @return all cache keys.
   */
  public Set<RuntimeID> getKeys() {
    return cache.keySet();
  }

  /** Clear the cached values. */
  public Set<RuntimeID> clear() {
    // Key Set is a **view** so clearing cache before returning
    // `keys` would always return an empty Set.
    var keys = new HashSet<>(cache.keySet());
    cache.clear();
    return keys;
  }

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

  public FunctionCallInstrumentationNode.FunctionCall enterable(UUID key) {
    return enterables.get(key);
  }

  public void updateEnterable(UUID key, FunctionCallInstrumentationNode.FunctionCall call) {
    enterables.put(key, call);
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
   * @return the preferences of this cache.
   */
  public CachePreferences getPreferences() {
    return preferences;
  }

  /**
   * Set the new cache preferences.
   *
   * @param preferences the new cache preferences
   */
  public void setPreferences(CachePreferences preferences) {
    this.preferences = preferences;
  }

  /**
   * Remove the cache preference associated with the provided key.
   *
   * @param key the preference to remove
   */
  public void removePreference(UUID key) {
    preferences.remove(key);
  }

  /** Clear the cache preferences. */
  public void clearPreferences() {
    preferences.clear();
  }

  /**
   * Executes a query while tracking access to the cache by {@code callback} observer.
   *
   * @param callback call with accessed UUIDs
   * @param scope the code to execute
   * @param <V> type of the returned value
   * @return value computed by the {@code scope}
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

  public int getId() {
    return id;
  }
}
