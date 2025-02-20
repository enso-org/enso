package org.enso.base.cache;

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
  private Value ensoReloadDetector;

  public ReloadDetector() {
    resetEnsoReloadDetector();
  }

  public boolean hasReloadOccurred() {
    var reloadHasOccurred = ensoReloadDetector.invokeMember("has_reload_occurred").asBoolean();
    if (reloadHasOccurred) {
      resetEnsoReloadDetector();
    }
    return reloadHasOccurred;
  }

  private void resetEnsoReloadDetector() {
    ensoReloadDetector =
        EnsoMeta.callStaticModuleMethod(
            "Standard.Base.Network.Reload_Detector", "create_reload_detector");
  }

  public void simulateReloadTestOnly() {
    EnsoMeta.callStaticModuleMethod(
        "Standard.Base.Network.Reload_Detector", "simulate_reload_test_only", ensoReloadDetector);
  }
}
