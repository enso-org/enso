package org.enso.interpreter.instrument;

import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Function;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.service.ExecutionService;

/**
 * Immutable API facade for Enso runtime cache. It contains only "query" methods. See {@link
 * Mutable} for operations that can modify the cache.
 *
 * <h3>Threading</h3>
 *
 * The methods provided by the {@code RuntimeCache} object are <em>thread safe</em> - e.g. they can
 * be called from any thread at any moment. However these methods serve only as
 * <strong>entrypoints</strong> for accessing the cache:
 *
 * <ul>
 *   <li><em>read only view</em> - there is {@link Immutable} interface providing read only view of
 *       the cache - it can only be used inside of the {@link #runQuery} method when provided as a
 *       callback interface
 *   <li><em>mutable view</em> - there is {@link Mutable} interface allowing to perform
 *       modifications to the cache. It follows its own threading rules ... TBD
 * </ul>
 *
 * <p>Implementation is provided by original implementation in {@link RuntimeCacheImpl}.
 */
public abstract class RuntimeCache {
  /** the only implementation is in the same package */
  RuntimeCache() {}

  /**
   * Factory method to create new runtime cache.
   *
   * @return mutable (e.g. privileged) interface to the cache
   */
  public static RuntimeCache.Mutable create() {
    return new RuntimeCacheImpl();
  }

  /**
   * Executes a query while tracking access to the cache by {@code callback} observer.
   *
   * @param callback call with accessed UUIDs
   * @param action the code to execute
   * @return value computed by the {@code scope}
   * @param <V> type of the returned value
   */
  public abstract <V> V runQuery(Consumer<UUID> callback, Function<Immutable, V> action);

  /** Immutable view of the cache. */
  public interface Immutable {
    /**
     * Accessor to the generic interface of the cache.
     *
     * @return associated instance of {@link RuntimeCache}
     */
    public abstract RuntimeCache cache();

    /**
     * Reads a cached value from the cache for given key. Cached values are hold be "soft reference"
     * - e.g. they are kept until the system runs out of memory or until the value for given key is
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
     * Obtains a list of UUIDs known to the cache.
     *
     * @param calls collect UUIDs of the calls
     * @param preferences collect UUIDs of the preferences
     * @return
     */
    public abstract Set<UUID> findUUIDs(boolean calls, boolean preferences);

    /**
     * Returns a functional metadata that has been previously cached.
     *
     * @param key UUID of the expression representing the function call
     * @return function call metadata
     */
    public FunctionCallInstrumentationNode.FunctionCall enterable(UUID key);
  }

  /**
   * Mutable (e.g. priviledged) view of the {@link RuntimeCache}. Created by {@link
   * RuntimeCache#create} method and held by those who perform execution and update the state of the
   * cache.
   *
   * <p>All other parties in the system that need just to observe the cache shall hold on {@link
   * RuntimeCache} only.
   */
  public interface Mutable {
    /**
     * Accessor to the generic interface of the cache.
     *
     * @return associated instance of {@link RuntimeCache}
     */
    public abstract RuntimeCache cache();

    /** Clears the content of the cache. */
    public abstract void clear();

    /**
     * Checks whether this key is associated with a binding expression.
     *
     * @param uuid the key to check
     * @return {@code true} or {@code false}
     */
    public abstract boolean isBindingExpression(UUID uuid);

    /**
     * Reads a cached value from the cache for given key. Cached values are hold be "soft reference"
     * - e.g. they are kept until the system runs out of memory or until the value for given key is
     * replacd
     *
     * @param key the UUID of the key
     * @return cached value associated with the {@code key} or {@code null}
     */
    public abstract Object get(UUID key);

    /**
     * Add value to the cache if it is possible.
     *
     * @param key the key of an entry.
     * @param value the added value.
     * @return {@code true} if the value was added to the cache.
     */
    public CacheOfferResult offer(UUID key, Object value);

    /**
     * Cache the type of expression.
     *
     * @return the previously cached type.
     */
    public TypeInfo putType(UUID key, TypeInfo typeInfo);

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
     * Cache the function call
     *
     * @param key the expression associated with the function call.
     * @param call the function call.
     * @return the function call that was previously associated with this expression.
     */
    public ExecutionService.FunctionCallInfo putCall(
        UUID key, ExecutionService.FunctionCallInfo call);

    /**
     * Registers metadata info about a function call encountered during execution.
     *
     * @param key UUID of the expression representing the function call
     * @param call metadata of the cached function call
     */
    public void updateEnterable(UUID key, FunctionCallInstrumentationNode.FunctionCall call);
  }

  /**
   * Encapsulates the result of calling {@code offer} method on {@code RuntimeCache}.
   *
   * @param canCache true if indicates that a value can be cached, false otherwise
   * @param updated true if a proposed value has been stored in the cache and no previous entry
   *     existed for the given key, false otherwise
   */
  public record CacheOfferResult(boolean canCache, boolean updated) {}
}
