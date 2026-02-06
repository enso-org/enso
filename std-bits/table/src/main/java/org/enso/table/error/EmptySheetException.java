package org.enso.table.error;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

public class EmptySheetException extends RuntimeException {
  public EmptySheetException() {
    super("Cannot parse an empty sheet.");
  }

  public Value asEnsoValue() {
    return EnsoMeta.makeInstance("Standard.Table.Errors", "Empty_Sheet", "Error");
  }
}
