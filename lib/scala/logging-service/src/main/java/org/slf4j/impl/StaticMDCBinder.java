package org.slf4j.impl;

import org.slf4j.helpers.NOPMDCAdapter;
import org.slf4j.spi.MDCAdapter;

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
