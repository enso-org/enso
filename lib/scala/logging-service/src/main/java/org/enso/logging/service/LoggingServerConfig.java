package org.enso.logging.service;

import java.net.URI;
import org.slf4j.event.Level;

/**
 * Configuration for socket appenders where logs should be sent to.
 *
 * @param minLogLevel minimal required log level to be logged in socket appender
 * @param uri uri of the logging server
 */
public record LoggingServerConfig(Level minLogLevel, URI uri) {}
