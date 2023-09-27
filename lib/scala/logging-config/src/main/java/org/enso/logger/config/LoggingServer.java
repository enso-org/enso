package org.enso.logger.config;

import com.typesafe.config.Config;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Configuration for the local server that collects logs from different services.
 *
 * @param port port of the local server that accepts logs
 * @param appender appender's configuration describing how to transform received log events
 * @param start if true, will be started by the service defining the configuration
 */
public record LoggingServer(int port, Map<String, Appender> appenders, String appender, boolean start, boolean logToFile) implements BaseConfig {

    public static final String startKey = "start";

    public static final String portKey = "port";


    public static LoggingServer parse(Config config) throws MissingConfigurationField {
        int port = config.getInt(portKey);

        Map<String, Appender> appendersMap = new HashMap<>();
        if (config.hasPath(LoggingServiceConfig.appendersKey)) {
            List<? extends Config> configs = config.getConfigList(LoggingServiceConfig.appendersKey);
            for (Config c : configs) {
                Appender a = Appender.parse(c);
                appendersMap.put(a.getName(), a);
            }
        }
        String defaultAppender = config.getString(LoggingServiceConfig.defaultAppenderKey);
        boolean start = config.hasPath(startKey) ? config.getBoolean(startKey) : false;
        boolean logToFile = config.hasPath(LoggingServiceConfig.alwaysLogToFileKey) ? config.getBoolean(LoggingServiceConfig.alwaysLogToFileKey) : false;
        return new LoggingServer(port, appendersMap, defaultAppender, start, logToFile);
    }

    @Override
    public Appender getAppender() {
        return appenders.get(appender);
    }

    @Override
    public Map<String, Appender> getAppenders() {
        return appenders;
    }

    @Override
    public LoggersLevels getLoggers() {
        return null;
    }
}
