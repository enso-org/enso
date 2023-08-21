package org.enso.logger.config;

import java.nio.file.Path;
import org.slf4j.event.Level;

public abstract class AppenderSetup {

  public abstract boolean setupSocketAppender(Level logLevel, String hostname, int port);

  public abstract boolean setupFileAppender(Level logLevel, Path logRoot, String logPrefix);

  public abstract boolean setupConsoleAppender(Level logLevel);

  public abstract boolean setupSentryAppender(Level logLevel, String dsn);

  public abstract boolean setupNoOpAppender();
}
