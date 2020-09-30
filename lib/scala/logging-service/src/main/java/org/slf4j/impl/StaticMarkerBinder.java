package org.slf4j.impl;

import org.slf4j.IMarkerFactory;
import org.slf4j.helpers.BasicMarkerFactory;

/**
 * Provides a simple marker factory for the SLF4J backend.
 * <p>
 * The public interface of this class must conform to what is expected by an SLF4J backend. See
 * slf4j-simple for reference.
 */
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
