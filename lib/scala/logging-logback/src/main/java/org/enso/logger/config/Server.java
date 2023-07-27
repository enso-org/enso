package org.enso.logger.config;

import com.typesafe.config.Config;

public record Server(String hostname, int port) {

    public static Server parse(Config config) {
        int port = config.getInt("port");
        String hostname = config.getString("hostname");
        return new Server(hostname, port);
    }
}
