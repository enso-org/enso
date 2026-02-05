package org.enso.interpreter.instrument;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Supplier;
import org.enso.common.CachePreferences;
import org.enso.interpreter.service.ExecutionService;

/**
 * Immutable API facade for Enso runtime cache. It contains only "query" methods. See {@link
 * Mutable} for operations that can modify the cache.
 *
 * <p>Implementation is provided by original implementation in {@link RuntimeCacheImpl}.
 */
public abstract class RuntimeCache {
  /** the only implementation is in the same package */
  RuntimeCache() {}

  /** Factory method to create new runtime cache. */
  public static RuntimeCache create() {
    return new RuntimeCacheImpl();
  }

  /**
   * Reads a cached value from the cache for given key. Cached values are hold be "soft reference" -
   * e.g. they are kept until the system runs out of memory or until the value for given key is
   * replacd
   *
   * @param key the UUID of the key
   * @return cached value associated with the {@code key} or {@code null}
   */
  public abstract Object get(UUID key);

  /**
   * Reads a value that was notified to the cache (which may or may not be subject to caching). A
   * non-cached values include values of expression which are only available while the computation
   * is running.
   *
   * @param key the UUID of the key
   * @return available value or {@code null}
   */
  public abstract Object getAnyValue(UUID key);

  /**
   * Obtains info about a type of expression identify by UUID.
   *
   * @param key the UUID of the key
   * @return the cached type of the expression or {@code null}
   */
  public abstract TypeInfo getType(UUID key);

  /**
   * Obtains function call for given UUID.
   *
   * @param key the UUID of the key
   * @return the cached function call associated with the expression or {@code null}
   */
  public abstract ExecutionService.FunctionCallInfo getCall(UUID key);

  /**
   * Executes a query while tracking access to the cache by {@code callback} observer.
   *
   * @param callback call with accessed UUIDs
   * @param scope the code to execute
   * @return value computed by the {@code scope}
   * @param <V> type of the returned value
   */
  public abstract <V> V runQuery(Consumer<UUID> callback, Supplier<V> scope);

  /**
   * Obtains a list of UUIDs known to the cache.
   *
   * @param calls collect UUIDs of the calls
   * @param preferences collect UUIDs of the preferences
   * @return
   */
  public final Set<UUID> findUUIDs(boolean calls, boolean preferences) {
    var impl = (RuntimeCacheImpl) this;
    var set = new HashSet<UUID>();
    if (calls) {
      set.addAll(impl.getCalls());
    }
    if (preferences) {
      set.addAll(impl.getPreferences().preferences().keySet());
    }
    return set;
  }

  /**
   * Checks whether this key is associated with a binding expression.
   *
   * @param uuid the key to check
   * @return {@code true} or {@code false}
   */
  public final boolean isBindingExpression(UUID uuid) {
    var impl = (RuntimeCacheImpl) this;
    return impl.getPreferences().get(uuid) == CachePreferences.Kind.BINDING_EXPRESSION;
  }

  /** */
  public interface Mutable {
    public abstract void clear();

    /**
     * Add value to the cache if it is possible.
     *
     * @param key the key of an entry.
     * @param value the added value.
     * @return {@code true} if the value was added to the cache.
     */
    public boolean offer(UUID key, Object value);

    /**
     * Cache the type of expression.
     *
     * @return the previously cached type.
     */
    public TypeInfo putType(UUID key, TypeInfo typeInfo);

    /**
     * Cache the function call
     *
     * @param key the expression associated with the function call.
     * @param call the function call.
     * @return the function call that was previously associated with this expression.
     */
    public ExecutionService.FunctionCallInfo putCall(
        UUID key, ExecutionService.FunctionCallInfo call);
  }
}
