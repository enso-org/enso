package org.enso.polyglot;

import org.graalvm.options.OptionDescriptor;
import org.graalvm.options.OptionKey;

/** Container for Runtime Server related constants. */
public class RuntimeServerInfo {
  public static final String URI = "enso://runtime-server";
  public static final String INSTRUMENT_NAME = "enso-runtime-server";
  public static final String ENABLE_OPTION = INSTRUMENT_NAME + ".enable";

  public static final String JOB_PARALLELISM_OPTION = INSTRUMENT_NAME + ".jobParallelism";
  public static final OptionKey<Integer> JOB_PARALLELISM_KEY = new OptionKey<>(1);
  public static final OptionDescriptor JOB_PARALLELISM_DESCRIPTOR =
      OptionDescriptor.newBuilder(JOB_PARALLELISM_KEY, JOB_PARALLELISM_OPTION).build();
}
