package org.enso.logger.config;

import com.typesafe.config.Config;

public record LoggingServer(String hostname, int port, Appender appender, Boolean start) {

    public static LoggingServer parse(Config config) throws MissingConfigurationField {
        int port = config.getInt("port");
        String hostname = config.getString("hostname");
        Appender appender = Appender.parse(config.getConfig("appender"));
        boolean start = config.getBoolean("start");
        return new LoggingServer(hostname, port, appender, start);
    }

}
