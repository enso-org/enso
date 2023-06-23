package org.enso.database;

import com.oracle.truffle.api.TruffleLogger;

/** Provides a logger associated with the std-database helper library. */
public class DatabaseHelperLogger {
  /** Returns a logger associated with the std-database helper library. */
  public static TruffleLogger getLogger() {
    return TruffleLogger.getLogger("enso-std-database");
  }
}
