package org.enso.logger.config;

import com.typesafe.config.Config;
import org.slf4j.event.Level;

/**
 * If `enabled`, all logs up to `logLevel` level will be persisted in the log file in parallel to the default appender.
 */
public record LogToFile(boolean enabled, Level logLevel) {

    public static final String logToFileEnabledKey = "enable";
    public static final String logToFileLogLevelKey = "log-level";

    /**
     * Parses `log-to-file` configuration section.
     *
     * @param config application configuration section that determines logging to a file
     * @return parsed `log-to-file` configuration
     */
    public static LogToFile fromConfig(Config config) {
        if (config.hasPath(logToFileEnabledKey)) {
            Level lvl = config.hasPath(logToFileLogLevelKey) ? fromConfigValue(config.getString(logToFileLogLevelKey)) : Level.DEBUG;
            return new LogToFile(config.getBoolean(logToFileEnabledKey), lvl);
        } else {
            return disabled();
        }
    }

    /**
     * Returns configuration that disables parallel logging to a file.
     */
    public static LogToFile disabled() {
        return new LogToFile(false, Level.ERROR);
    }

    private static Level fromConfigValue(String logLevel) {
        try {
            return Level.valueOf(logLevel.toUpperCase());
        } catch (IllegalArgumentException iae) {
            return Level.DEBUG;
        }
    }
}

