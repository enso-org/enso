package org.enso.base.cache;

import java.util.Map;
import java.util.WeakHashMap;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

/**
 * Detects that the reload button has been pressed.
 *
 * <p>.hasReloadOccurred() returns true if the reload button was pressed since the last call to
 * .hasReloadOccurred().
 *
 * <p>This uses a `Managed_Resource` (created in eval'd Enso code) that is cleared on reload.
 */
public class ReloadDetector {
  public static final ReloadDetector INSTANCE = new ReloadDetector();

  private Map<HasClearableCache, ReloadSentinel> registrations = new WeakHashMap<>();

  public void register(HasClearableCache o) {
    registrations.put(o, new ReloadSentinel());
  }

  private ReloadSentinel getSentinel(HasClearableCache o) {
    if (!registrations.containsKey(o)) {
      throw new HasClearableCacheNotRegisteredException("Clearable cache object is not registered: " + o);
    }
    return registrations.get(o);
  }

  public boolean hasReloadOccurred(HasClearableCache o) {
    return getSentinel(o).hasReloadOccurred();
  }

  public void simulateReloadTestOnly(HasClearableCache o) {
    getSentinel(o).simulateReloadTestOnly();
  }

  public void clearOnReload(HasClearableCache o) {
    if (getSentinel(o).hasReloadOccurred())  {
      o.clearCache();
    }
  }

  public void clearOnReloadIfRegistered(HasClearableCache o) {
    if (registrations.containsKey(o)) {
      clearOnReload(o);
    }
  }

  public interface HasClearableCache {
    void clearCache();
  }

  private static class ReloadSentinel {
    private Value ensoReloadSentinel;

    public ReloadSentinel() {
      resetEnsoReloadSentinel();
    }

    public boolean hasReloadOccurred() {
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
      EnsoMeta.callStaticModuleMethod(
          "Standard.Base.Network.Reload_Sentinel", "simulate_reload_test_only", ensoReloadSentinel);
    }
  }
}
