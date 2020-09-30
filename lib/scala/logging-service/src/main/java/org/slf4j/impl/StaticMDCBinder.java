package org.slf4j.impl;

import org.slf4j.helpers.NOPMDCAdapter;
import org.slf4j.spi.MDCAdapter;

/**
 * Provides a no-op MDC adapter for the SLF4J backend.
 * <p>
 * MDC handling is an optional SLF4J feature and currently the logging service does not support it,
 * so it provides a no-op adapter.
 * <p>
 * The public interface of this class must conform to what is expected by an SLF4J backend. See
 * slf4j-simple for reference.
 */
public class StaticMDCBinder {

  final private static StaticMDCBinder singleton = new StaticMDCBinder();

  public static StaticMDCBinder getSingleton() {
    return singleton;
  }

  private final MDCAdapter adapter = new NOPMDCAdapter();
  private final String adapterClassStr = NOPMDCAdapter.class.getName();

  public MDCAdapter getMDCA() {
    return adapter;
  }

  public String getMDCAdapterClassStr() {
    return adapterClassStr;
  }
}
