package org.enso.interpreter.service.error;

import org.enso.logger.masking.MaskedString;
import org.enso.pkg.QualifiedName;
import org.enso.text.editing.model;
import scala.collection.immutable.Seq;

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
      QualifiedName module, Seq<model.TextEdit> edits, Object failure, Object source) {
    super(
        "Failed to apply edits for "
            + module
            + ", edits="
            + edits
                .map(edit -> edit.copy(edit.range(), new MaskedString(edit.text()).applyMasking()))
                .toString()
            + ", failure="
            + failure
            + ", source='"
            + new MaskedString(source.toString()).applyMasking()
            + "'");
  }
}
