package org.slf4j.impl;

import org.enso.loggingservice.WSLoggerFactory;
import org.slf4j.ILoggerFactory;

public class StaticLoggerBinder {
  public static String REQUESTED_API_VERSION = "1.7.30";

  final private static StaticLoggerBinder singleton = new StaticLoggerBinder();

  public static StaticLoggerBinder getSingleton() {
    return singleton;
  }

  private final WSLoggerFactory factory = new WSLoggerFactory();
  private final String factoryClassStr = WSLoggerFactory.class.getName();


  public ILoggerFactory getLoggerFactory() {
    return factory;
  }

  public String getLoggerFactoryClassStr() {
    return factoryClassStr;
  }
}
