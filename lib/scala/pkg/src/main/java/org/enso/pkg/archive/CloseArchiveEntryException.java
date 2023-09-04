package org.enso.pkg.archive;

import java.io.IOException;

/** Error when trying to close an enso-project archive entry. */
public final class CloseArchiveEntryException extends EnsoProjectArchiveException {

  public CloseArchiveEntryException(IOException cause) {
    super(cause);
  }
}
