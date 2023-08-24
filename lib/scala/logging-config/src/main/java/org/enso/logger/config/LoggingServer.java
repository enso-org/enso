package org.enso.logger.config;

import com.typesafe.config.Config;

/**
 * Configuration for the local server that collects logs from different services.
 *
 * @param port port of the local server that accepts logs
 * @param appender appender's configuration describing how to transform received log events
 * @param start if true, will be started by the service defining the configuration
 */
public record LoggingServer(int port, Appender appender, Boolean start) {

    public static LoggingServer parse(Config config) throws MissingConfigurationField {
        int port = config.getInt("port");
        Appender appender = Appender.parse(config.getConfig("appender"));
        boolean start = config.getBoolean("start");
        return new LoggingServer(port, appender, start);
    }

}
