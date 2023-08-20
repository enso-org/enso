package org.enso.logger.config;

import com.typesafe.config.Config;

public record Server(String hostname, int port, Appender appender, Boolean start) {

    public static Server parse(Config config) {
        int port = config.getInt("port");
        String hostname = config.getString("hostname");
        Appender appender = Appender.parse(config.getConfig("appender"));
        boolean start = config.getBoolean("start");
        return new Server(hostname, port, appender, start);
    }

}
