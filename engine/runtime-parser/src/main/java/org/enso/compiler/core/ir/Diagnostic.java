package org.enso.compiler.core.ir;

import scala.Function1;
import scala.Option;

/** A representation of various kinds of diagnostic in the IR. */
public interface Diagnostic {

  /**
   * @param source Location of the diagnostic.
   * @return a human-readable description of this error condition.
   */
  String message(Function1<IdentifiedLocation, String> source);

  /**
   * @param source Location of the diagnostic.
   * @return a human-readable description of this error condition, formatted for immediate
   *     reporting.
   */
  default String formattedMessage(Function1<IdentifiedLocation, String> source) {
    return message(source);
  }

  /** The location at which the diagnostic occurs. */
  IdentifiedLocation identifiedLocation();

  /** The location at which the diagnostic occurs. */
  default Option<IdentifiedLocation> location() {
    return Option.apply(identifiedLocation());
  }

  /** The important keys identifying identity of the diagnostic */
  Object[] diagnosticKeys();

  /** Represents the various kinds of diagnostics in the IR. */
  sealed interface Kind permits Kind.Static, Kind.Interactive {

    /** Diagnostics that should be reported during the static compilation phase of execution. */
    non-sealed interface Static extends Kind {}

    /** Diagnostics that should remain at runtime for display during interactive execution. */
    non-sealed interface Interactive extends Kind {}
  }
}
