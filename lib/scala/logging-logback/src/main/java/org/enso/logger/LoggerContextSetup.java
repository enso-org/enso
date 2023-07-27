package org.enso.logger;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.joran.JoranConfigurator;
import ch.qos.logback.core.joran.spi.JoranException;
import ch.qos.logback.core.util.StatusPrinter;
import java.io.InputStream;
import org.enso.logger.config.LoggingService;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;

public class LoggerContextSetup {

  public static Boolean setupLogging(
      Level level, String componentName, InputStream customLogConfig) {
    return setupLogging(level, componentName, customLogConfig, LoggingService.parseConfig());
  }

  public static Boolean setupLogging(
      Level logLevel, String componentName, InputStream customLogConfig, LoggingService appConfig) {
    var context = (LoggerContext) LoggerFactory.getILoggerFactory();
    var logbackLevel = ch.qos.logback.classic.Level.convertAnSLF4JLevel(logLevel);
    System.setProperty(componentName + ".logLevel", logbackLevel.toString().toLowerCase());
    System.setProperty(componentName + ".appender", appConfig.getAppender());
    var serverConfig = appConfig.getServer();
    System.setProperty("logging-server.host", serverConfig.hostname());
    System.setProperty("logging-server.port", String.valueOf(serverConfig.port()));
    try {
      var configurator = new JoranConfigurator();
      configurator.setContext(context);
      // Call context.reset() to clear any previous configuration, e.g. default
      // configuration.
      context.reset();
      configurator.doConfigure(customLogConfig);
      var rootLogger = context.getLogger("root");
      var appender = rootLogger.getAppender(appConfig.getAppender());
      if (appender == null) {
        System.err.println(
            "Failed to apply custom log levels for application loggers' in "
                + appConfig.getAppender());
      } else {
        var filter = ApplicationFilter.fromLoggers(appConfig.getLoggers());
        appender.addFilter(filter);
      }
      return true;
    } catch (JoranException je) {
      je.printStackTrace();
      StatusPrinter.printInCaseOfErrorsOrWarnings(context);
      System.err.println("Failed to setup Logback configuration");
      return false;
    }
  }
}
