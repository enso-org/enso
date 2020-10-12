package org.enso.interpreter.service.error;

import java.io.File;

/** Thrown when the edits can not be applied to the source. */
public class FailedToApplyEditsException extends RuntimeException implements ServiceException {

  /**
   * Create new instance of this error.
   *
   * @param path the source file path.
   */
  public FailedToApplyEditsException(File path) {
    super("Filed to apply edits for file " + path + ".");
  }
}
