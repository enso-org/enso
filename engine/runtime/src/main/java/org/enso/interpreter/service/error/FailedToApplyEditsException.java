package org.enso.interpreter.service.error;

import org.enso.logger.masking.MaskedString;
import org.enso.pkg.QualifiedName;

/** Thrown when the edits can not be applied to the source. */
public class FailedToApplyEditsException extends RuntimeException implements ServiceException {

  /**
   * Create new instance of this error.
   *
   * @param module the edited module.
   * @param edits the applied edits.
   * @param failure the failure object.
   * @param source the source text.
   */
  public FailedToApplyEditsException(
      QualifiedName module, Object edits, Object failure, Object source) {
    super(
        "Failed to apply edits for "
            + module
            + ", edits="
            + new MaskedString(edits.toString()).applyMasking()
            + ", failure="
            + failure
            + ", source='"
            + new MaskedString(source.toString()).applyMasking()
            + "'");
  }
}
