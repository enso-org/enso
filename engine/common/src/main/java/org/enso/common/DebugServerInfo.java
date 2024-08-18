package org.enso.common;

/** Container for debug server related constants. */
public class DebugServerInfo {
  private DebugServerInfo() {

  }
  public static final String URI = "enso://debug-server";
  public static final String INSTRUMENT_NAME = "enso-debug-server";
  public static final String ENABLE_OPTION = INSTRUMENT_NAME + ".enable";
  public static final String FN_OPTION = INSTRUMENT_NAME + ".fn";
}
