package org.enso.interpreter;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/**
 * An exception thrown to break out of the compilation flow after reporting all the encountered
 * errors.
 */
@ExportLibrary(InteropLibrary.class)
public final class CompilationAbortedException extends AbstractTruffleException {
  public static final String COMPILATION_ABORTED_MSG = "Compilation aborted due to errors.";

  @Override
  public String getMessage() {
    return COMPILATION_ABORTED_MSG;
  }

  @ExportMessage
  ExceptionType getExceptionType() {
    return ExceptionType.PARSE_ERROR;
  }
}
