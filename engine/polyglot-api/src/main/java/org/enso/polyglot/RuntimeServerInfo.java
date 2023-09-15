package org.enso.polyglot;

import org.graalvm.options.OptionKey;

/** Container for Runtime Server related constants. */
public class RuntimeServerInfo {
  public static final String URI = "enso://runtime-server";
  public static final String INSTRUMENT_NAME = "enso-runtime-server";
  public static final String ENABLE_OPTION = INSTRUMENT_NAME + ".enable";
  public static final OptionKey<String> ENABLE_OPTION_KEY = new OptionKey<>("");
}
