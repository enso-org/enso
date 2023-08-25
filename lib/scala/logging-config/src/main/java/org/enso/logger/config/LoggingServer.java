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
public record LoggingServer(int port, Map<String, Appender> appenders, String appender, Boolean start) {

    public static LoggingServer parse(Config config) throws MissingConfigurationField {
        int port = config.getInt("port");

        Map<String, Appender> appendersMap = new HashMap<>();
        if (config.hasPath("appenders")) {
            List<? extends Config> configs = config.getConfigList("appenders");
            for (Config c : configs) {
                Appender a = Appender.parse(c);
                appendersMap.put(a.getName(), a);
            }
        }
        String defaultAppender = config.getString("default-appender");
        boolean start = config.getBoolean("start");
        return new LoggingServer(port, appendersMap, defaultAppender, start);
    }

}
