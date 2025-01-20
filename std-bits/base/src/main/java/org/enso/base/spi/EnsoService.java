package org.enso.base.spi;

import java.util.logging.Logger;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;

/** A base class for an Enso service backed by an Enso type. */
public abstract class EnsoService {
  /**
   * A cached Value representing the loaded type object.
   *
   * <p>If it is set to {@code null} it means it has not been loaded yet. If it cannot be loaded it
   * is set to a Value instance that {@code isNull}.
   */
  private transient Value cachedTypeObject = null;

  /**
   * Defines the path of the Enso module that defines the type associated with this SPI
   * registration.
   */
  protected abstract String getModuleName();

  /** Defines the name of the type associated with this SPI registration. */
  protected abstract String getTypeName();

  /**
   * Resolved the Enso type object associated with this SPI registration and returns it as a Value.
   *
   * <p>If the Enso library associated with the service is not loaded, it returns a Polyglot Value
   * which {@code isNull}.
   */
  public final Value getTypeObject() {
    if (cachedTypeObject == null) {
      try {
        cachedTypeObject = EnsoMeta.getType(getModuleName(), getTypeName());
      } catch (PolyglotException e) {
        // Currently I have not found a way to get the type/class of the exception, so we rely on
        // the message.
        boolean isModuleNotLoaded =
            e.getMessage().equals("Module " + getModuleName() + " does not exist.");
        if (isModuleNotLoaded) {
          Logger.getLogger(this.getClass().getCanonicalName())
              .warning(
                  "Failed to instantiate type object for "
                      + this.getClass().getCanonicalName()
                      + ": "
                      + e.getMessage());
          cachedTypeObject = Value.asValue(null);
          assert cachedTypeObject.isNull();
        } else {
          throw e;
        }
      }
    }

    assert cachedTypeObject != null;
    return cachedTypeObject;
  }

  /** Returns whether the Enso library providing the associated type is loaded. */
  public boolean isLoaded() {
    boolean unavailable = getTypeObject().isNull();
    return !unavailable;
  }
}
