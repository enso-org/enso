package org.enso.logger.config;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import java.lang.reflect.Method;

public class LoggerSetupFactory {

  private static LoggerSetup _loaded = null;

  public static LoggerSetup get() {
    if (_loaded == null) {
      Config c = ConfigFactory.load("enso-logging.conf");
      if (c.hasPath(implClassKey)) {
        try {
          String clazzName = c.getString(implClassKey);
          Class<?> clazz = Class.forName(clazzName);
          Method meth = clazz.getDeclaredMethod("get");
          _loaded = (LoggerSetup) meth.invoke(null);
        } catch (Throwable e) {
          e.printStackTrace();
          System.err.println("Failed to initialize LoggerSetup configuration class");
          return null;
        }
      } else {
        System.err.println("Missing log configuration class key:" + implClassKey);
        return null;
      }
    }
    return _loaded;
  }

  private static final String implClassKey = LoggerSetup.class.getName() + ".impl.class";
}
