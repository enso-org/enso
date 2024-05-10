package org.enso.interpreter;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.SourceSection;

/**
 * An exception thrown to break out of the compilation flow after reporting all the encountered
 * errors.
 */
@ExportLibrary(InteropLibrary.class)
public final class CompilationAbortedException extends AbstractTruffleException {
  private final SourceSection where;

  public CompilationAbortedException(String msg, SourceSection where) {
    super(msg);
    this.where = where;
  }

  public CompilationAbortedException() {
    this("Compilation aborted due to errors.", null);
  }

  @ExportMessage
  ExceptionType getExceptionType() {
    return ExceptionType.PARSE_ERROR;
  }

  @ExportMessage
  boolean hasSourceLocation() {
    return where != null;
  }

  @ExportMessage
  SourceSection getSourceLocation() {
    return where;
  }
}
