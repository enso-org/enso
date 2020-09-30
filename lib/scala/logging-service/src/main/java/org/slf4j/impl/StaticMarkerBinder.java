package org.slf4j.impl;

import org.slf4j.IMarkerFactory;
import org.slf4j.helpers.BasicMarkerFactory;

public class StaticMarkerBinder {
  final private static StaticMarkerBinder singleton = new StaticMarkerBinder();

  public static StaticMarkerBinder getSingleton() {
    return singleton;
  }

  private final IMarkerFactory markerFactory = new BasicMarkerFactory();
  private final String markerFactoryClassStr =
      BasicMarkerFactory.class.getName();

  public IMarkerFactory getMarkerFactory() {
    return markerFactory;
  }

  public String getMarkerFactoryClassStr() {
    return markerFactoryClassStr;
  }
}
