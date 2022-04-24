package org.enso.logger.masking;

/** Java API providing access to the masking module. */
public class MaskingFactory {

  /** @return the masking instance. */
  public static Masking$ getInstance() {
    return Masking$.MODULE$;
  }
}
