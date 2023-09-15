package org.enso.pkg.archive;

import java.io.IOException;

/** Error when trying to write an enso-project archive entry. */
public final class WriteArchiveEntryException extends EnsoProjectArchiveException {

  public WriteArchiveEntryException(IOException cause) {
    super(cause);
  }
}
