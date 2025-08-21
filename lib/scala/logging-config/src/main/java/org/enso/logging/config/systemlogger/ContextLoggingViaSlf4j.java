package org.enso.logging.config.systemlogger;

import java.util.logging.Handler;
import java.util.logging.Level;
import org.enso.common.ContextLoggingConfigurator;
import org.enso.common.LanguageInfo;
import org.enso.logger.Converter;
import org.enso.logger.JulHandler;
import org.enso.logging.config.LoggerSetup;
import org.graalvm.polyglot.Context;

/**
 * Provides implementation of the {@link ContextLoggingConfigurator} for the `engine-common` module.
 */
public final class ContextLoggingViaSlf4j extends ContextLoggingConfigurator {
  public ContextLoggingViaSlf4j() {}

  /** Configures provided builder via {@link LoggerSetup}. */
  @Override
  protected final void prepareBuilderForLogging(
      Context.Builder builder, Level julLogLevel, Handler logHandler) {

    var logLevelName = julLogLevel.getName();

    builder.option(org.enso.common.RuntimeOptions.LOG_LEVEL, logLevelName);
    var logLevels = LoggerSetup.get().getConfig().getLoggers();
    if (logLevels.hasEnsoLoggers()) {
      logLevels
          .entrySet()
          .forEach(
              (entry) ->
                  builder.option(
                      "log." + LanguageInfo.ID + "." + entry.getKey() + ".level",
                      Converter.toJavaLevel(entry.getValue()).getName()));
    }
    builder.logHandler(logHandler != null ? logHandler : JulHandler.get());
  }
}
