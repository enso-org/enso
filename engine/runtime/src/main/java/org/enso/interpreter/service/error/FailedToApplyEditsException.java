package org.enso.interpreter.service.error;

import org.enso.logger.masking.MaskedPath;
import org.enso.logger.masking.MaskedString;

import java.io.File;

/** Thrown when the edits can not be applied to the source. */
public class FailedToApplyEditsException extends RuntimeException implements ServiceException {

  /**
   * Create new instance of this error.
   *
   * @param path the source file path.
   * @param edits the applied edits.
   * @param failure the failure object.
   * @param source the source text.
   */
  public FailedToApplyEditsException(File path, Object edits, Object failure, Object source) {
    super(
        "Filed to apply edits for file "
            + new MaskedPath(path.toPath()).applyMasking()
            + ", edits="
            + new MaskedString(edits.toString()).applyMasking()
            + ", failure="
            + failure
            + ", source='"
            + new MaskedString(source.toString()).applyMasking()
            + "'");
  }
}
