package org.enso.interpreter.epb;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleOptions;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import java.util.SortedSet;

/**
 * This exception is thrown once a foreign method cannot be parsed because the associated foreign
 * language is not installed, or enabled, in the Truffle engine.
 */
@ExportLibrary(InteropLibrary.class)
class ForeignParsingException extends AbstractTruffleException {
  private final String message;

  /**
   * @param truffleLangId ID of the language that caused the parsing exception, i.e., ID of the
   *     language that is not installed in the GraalVM distribution.
   * @param installedLangs Set of all the installed (supported) language IDs in the GraalVM
   *     distribution.
   * @param location Location node passed to {@link AbstractTruffleException}.
   */
  ForeignParsingException(String truffleLangId, SortedSet<String> installedLangs, Node location) {
    this(createMessage(truffleLangId, installedLangs), location);
  }

  /**
   * @param msg message of the exception
   * @param location Location node passed to {@link AbstractTruffleException}.
   */
  ForeignParsingException(String msg, Node location) {
    super(msg, location);
    this.message = msg;
  }

  @TruffleBoundary
  private static String createMessage(String truffleLangId, Iterable<String> installedLangs) {
    var allLangs = String.join(", ", installedLangs);
    var format = "Cannot parse `foreign %s` method. Only available languages are %s. %s";
    var extraInfo = "";
    if (TruffleOptions.AOT) {
      extraInfo = "\nMore languages may be available in JVM mode. Try running with --jvm option.";
    }
    return String.format(format, truffleLangId, allLangs, extraInfo);
  }

  @Override
  public String getMessage() {
    return message;
  }

  @ExportMessage
  boolean isException() {
    return true;
  }

  @ExportMessage
  RuntimeException throwException() {
    return this;
  }

  @ExportMessage
  @TruffleBoundary
  String toDisplayString(boolean hasSideEffects) {
    return "ForeignParsingException: '" + message + "'";
  }
}
