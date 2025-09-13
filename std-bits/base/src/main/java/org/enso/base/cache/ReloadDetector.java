package org.enso.base.cache;

import java.util.Map;
import java.util.WeakHashMap;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

/**
 * Register caches so they can be cleared when the reload button is pressed.
 *
 * <p>Cache clearing does not happen automatically in the background. A cache must implement
 * HasClearableCache, register itself, and then poll using ReloadDetector.clearOnReload(this), which
 * will invoke the clearCache() callback if a reload has just happenend. Thus, a client cache
 * decides exactly when it wants caches to be cleared.
 *
 * <p>If clearOnReload() is called on an object that wasn't registered, an exception is thrown. A
 * cache object that doesn't know if it was registered can safely call clearOnReloadIfRegistered()
 * on itself in this case.
 */
public class ReloadDetector {
  /**
   * Internally, a separate ReloadSentinel is created for each registration. The sentinel's
   * hasReloadOccurred() method will return true exactly one time, for that cache, after a reload
   * has occurred.
   */
  private static Map<HasClearableCache, ReloadSentinel> registrations = new WeakHashMap<>();

  public static void register(HasClearableCache o) {
    registrations.put(o, new ReloadSentinel());
  }

  public static void clearOnReload(HasClearableCache o) {
    if (getSentinel(o).hasReloadOccurred()) {
      o.clearCache();
    }
  }

  public static void clearOnReloadIfRegistered(HasClearableCache o) {
    if (registrations.containsKey(o)) {
      clearOnReload(o);
    }
  }

  public static void simulateReloadTestOnly(HasClearableCache o) {
    getSentinel(o).simulateReloadTestOnly();
  }

  private static ReloadSentinel getSentinel(HasClearableCache o) {
    if (!registrations.containsKey(o)) {
      throw new HasClearableCacheNotRegisteredException(
          "Clearable cache object is not registered: " + o);
    }
    return registrations.get(o);
  }

  public interface HasClearableCache {
    void clearCache();
  }

  /**
   * Detects that the reload button has been pressed.
   *
   * <p>.hasReloadOccurred() returns true if the reload button was pressed since the last call to
   * .hasReloadOccurred().
   *
   * <p>This uses a `Managed_Resource` (created in eval'd Enso code) that is cleared on reload.
   */
  private static class ReloadSentinel {
    private Value ensoReloadSentinel;
    private boolean initialized;

    public ReloadSentinel() {
      initialized = false;
    }

    private void ensureInitialized() {
      if (!initialized) {
        resetEnsoReloadSentinel();
      }
      initialized = true;
    }

    public boolean hasReloadOccurred() {
      ensureInitialized();
      var reloadHasOccurred = ensoReloadSentinel.invokeMember("has_reload_occurred").asBoolean();
      if (reloadHasOccurred) {
        resetEnsoReloadSentinel();
      }
      return reloadHasOccurred;
    }

    private void resetEnsoReloadSentinel() {
      ensoReloadSentinel =
          EnsoMeta.callStaticModuleMethod(
              "Standard.Base.Network.Reload_Sentinel", "create_reload_sentinel");
    }

    public void simulateReloadTestOnly() {
      ensureInitialized();
      EnsoMeta.callStaticModuleMethod(
          "Standard.Base.Network.Reload_Sentinel", "simulate_reload_test_only", ensoReloadSentinel);
    }
  }
}
