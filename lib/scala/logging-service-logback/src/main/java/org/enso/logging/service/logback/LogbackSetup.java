package org.enso.logging.service.logback;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import ch.qos.logback.classic.filter.ThresholdFilter;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.ConsoleAppender;
import ch.qos.logback.core.FileAppender;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.helpers.NOPAppender;
import ch.qos.logback.core.rolling.RollingFileAppender;
import ch.qos.logback.core.rolling.SizeAndTimeBasedRollingPolicy;
import ch.qos.logback.core.spi.FilterReply;
import ch.qos.logback.core.util.Duration;
import ch.qos.logback.core.util.FileSize;
import java.io.File;
import java.net.URI;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import org.enso.logging.config.*;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;

public final class LogbackSetup extends LoggerSetup {

  private static final String CONSOLE_APPENDER_NAME = "enso-console";
  private static final String TELEMETRY_ROOT_LOGGER = "org.enso.telemetry";

  private LogbackSetup(LoggingServiceConfig config, LoggerContext context) {
    this.config = config;
    this._context = context;
  }

  public LogbackSetup() throws MissingConfigurationField {
    this(LoggingServiceConfig.parseConfig(), (LoggerContext) LoggerFactory.getILoggerFactory());
  }

  public LogbackSetup(LoggerContext ctx) throws MissingConfigurationField {
    this(LoggingServiceConfig.parseConfig(), ctx);
  }

  /**
   * Create a logger setup for a provided context and a single appender configuration
   *
   * @param context context that will be initialized by this setup
   * @param config configuration to use during initialization
   */
  public static LogbackSetup forContext(LoggerContext context, BaseConfig config) {
    return new LogbackSetup(LoggingServiceConfig.withSingleAppender(config), context);
  }

  public LoggingServiceConfig getConfig() {
    return config;
  }

  private boolean logToFileEnabled() {
    return config.logToFile().enabled();
  }

  private final LoggingServiceConfig config;
  private LoggerContext _context;

  private LoggerContext context() {
    if (_context == null) {
      _context = (LoggerContext) LoggerFactory.getILoggerFactory();
    }
    return _context;
  }

  @Override
  public boolean setup() throws MissingConfigurationField {
    LoggingServiceConfig config = LoggingServiceConfig.parseConfig();
    return setup(config);
  }

  private boolean setup(LoggingServiceConfig config) {
    Level defaultLogLevel =
        config
            .getLogLevel()
            .map(name -> Level.valueOf(name.toUpperCase()))
            .orElseGet(() -> Level.ERROR);
    return setup(defaultLogLevel, config);
  }

  @Override
  public boolean setup(Level logLevel) throws MissingConfigurationField {
    return setup(logLevel, LoggingServiceConfig.parseConfig());
  }

  public boolean setup(Level logLevel, LoggingServiceConfig config) {
    Appender defaultAppender = config.getAppender();
    if (defaultAppender != null) {
      return defaultAppender.setup(logLevel, this);
    } else {
      return setupConsoleAppender(logLevel);
    }
  }

  @Override
  public boolean setup(
      Level logLevel,
      Path componentLogPath,
      String componentLogPrefix,
      LoggingServiceConfig config) {
    Appender defaultAppender = config.getAppender();
    if (defaultAppender != null) {
      return defaultAppender.setupForPath(logLevel, componentLogPath, componentLogPrefix, this);
    } else {
      return setupConsoleAppender(logLevel);
    }
  }

  @Override
  public boolean setupSocketAppender(Level logLevel, String hostname, int port) {
    Level targetLogLevel;
    // Modify log level if we were asked to always log to a file.
    // The receiver needs to get all logs (up to `trace`) to be able to log all verbose messages.
    if (logToFileEnabled()) {
      int min = Math.min(logLevel.toInt(), config.logToFile().logLevel().toInt());
      targetLogLevel = Level.intToLevel(min);
    } else {
      targetLogLevel = logLevel;
    }
    LoggerAndContext env = contextInit(targetLogLevel, config, true);

    org.enso.logging.config.SocketAppender appenderConfig = config.getSocketAppender();

    DeferredProcessingSocketAppender socketAppender = new DeferredProcessingSocketAppender();
    socketAppender.setName("enso-socket");
    socketAppender.setRemoteHost(hostname);
    socketAppender.setPort(port);
    if (appenderConfig != null) {
      socketAppender.setReconnectionDelay(
          Duration.buildByMilliseconds(appenderConfig.getReconnectionDelay()));
    }

    acceptAllTelemetryEvents(socketAppender);
    env.finalizeAppender(socketAppender);
    return true;
  }

  private static void acceptAllTelemetryEvents(
      ch.qos.logback.core.Appender<ILoggingEvent> appender) {
    // This filter lets all the telemetry log events through.
    var telemetryAcceptingFilter =
        new Filter<ILoggingEvent>() {
          @Override
          public FilterReply decide(ILoggingEvent event) {
            if (event.getLoggerName().startsWith(TELEMETRY_ROOT_LOGGER)) {
              return FilterReply.ACCEPT;
            } else {
              return FilterReply.NEUTRAL;
            }
          }
        };
    appender.addFilter(telemetryAcceptingFilter);
  }

  @Override
  public boolean setupFileAppender(Level logLevel, Path logRoot, String logPrefix) {
    try {
      LoggerAndContext env = contextInit(logLevel, config, true);
      org.enso.logging.config.FileAppender appenderConfig = config.getFileAppender();
      if (appenderConfig == null) {
        throw new MissingConfigurationField(org.enso.logging.config.FileAppender.appenderName);
      }
      final PatternLayoutEncoder encoder = new PatternLayoutEncoder();
      encoder.setPattern(appenderConfig.getPattern());
      encoder.setContext(env.ctx);
      encoder.start();

      FileAppender<ILoggingEvent> fileAppender;

      if (appenderConfig != null && appenderConfig.getRollingPolicy() != null) {
        RollingFileAppender<ILoggingEvent> rollingFileAppender = new RollingFileAppender<>();
        fileAppender = rollingFileAppender;
        fileAppender.setContext(
            env.ctx); // Context needs to be set prior to rolling policy initialization
        String filePattern;
        if (logRoot == null || logPrefix == null) {
          filePattern = "enso-%d{yyyy-MM-dd}";
        } else {
          filePattern =
              logRoot.toAbsolutePath() + File.separator + logPrefix + "-" + "%d{yyyy-MM-dd}";
        }

        org.enso.logging.config.FileAppender.RollingPolicy rollingPolicy =
            appenderConfig.getRollingPolicy();
        SizeAndTimeBasedRollingPolicy logbackRollingPolicy = new SizeAndTimeBasedRollingPolicy();
        logbackRollingPolicy.setContext(env.ctx);
        logbackRollingPolicy.setParent(fileAppender);
        logbackRollingPolicy.setMaxFileSize(FileSize.valueOf(rollingPolicy.maxFileSize()));
        logbackRollingPolicy.setMaxHistory(rollingPolicy.maxHistory());
        logbackRollingPolicy.setTotalSizeCap(FileSize.valueOf(rollingPolicy.totalSizeCap()));
        logbackRollingPolicy.setFileNamePattern(filePattern + ".%i.log.gz");
        logbackRollingPolicy.start();

        rollingFileAppender.setRollingPolicy(logbackRollingPolicy);
      } else {
        fileAppender = new FileAppender<>();
        fileAppender.setName("enso-file");
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        String currentDate = LocalDate.now().format(dtf);
        String fullFilePath;
        if (logRoot == null || logPrefix == null) {
          fullFilePath = "enso-" + currentDate + ".log";
        } else {
          fullFilePath =
              logRoot.toAbsolutePath() + File.separator + logPrefix + "-" + currentDate + ".log";
        }
        fileAppender.setFile(fullFilePath);
      }

      fileAppender.setAppend(appenderConfig.isAppend());
      fileAppender.setImmediateFlush(appenderConfig.isImmediateFlush());
      fileAppender.setEncoder(encoder);

      env.finalizeAppender(fileAppender);
    } catch (Throwable e) {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  @Override
  public boolean setupConsoleAppender(Level logLevel) {
    LoggerAndContext env = contextInit(logLevel, config, !logToFileEnabled());
    var consoleAppender = getConsoleAppender(env.ctx, config);
    env.finalizeAppender(consoleAppender);
    return true;
  }

  private ch.qos.logback.core.Appender<ILoggingEvent> getConsoleAppender(
      LoggerContext ctx, LoggingServiceConfig config) {
    org.enso.logging.config.ConsoleAppender appenderConfig = config.getConsoleAppender();
    final PatternLayoutEncoder encoder = new PatternLayoutEncoder();
    try {
      if (appenderConfig != null) {
        encoder.setPattern(appenderConfig.getPattern());
      } else {
        encoder.setPattern(Appender.defaultPattern);
      }
    } catch (Throwable e) {
      e.printStackTrace();
      encoder.setPattern(Appender.defaultPattern);
    }
    encoder.setContext(ctx);
    encoder.start();

    ConsoleAppender<ILoggingEvent> consoleAppender = new ConsoleAppender<>();
    consoleAppender.setName(CONSOLE_APPENDER_NAME);
    consoleAppender.setEncoder(encoder);
    return consoleAppender;
  }

  @Override
  public boolean setupMemoryAppender(Level logLevel) {
    LoggerAndContext env = contextInit(logLevel, config, !logToFileEnabled());
    org.enso.logging.config.MemoryAppender appenderConfig = config.getMemoryAppender();
    ch.qos.logback.core.Appender<ILoggingEvent> target;
    switch (appenderConfig.getTarget()) {
      case org.enso.logging.config.ConsoleAppender.appenderName:
        target = getConsoleAppender(env.ctx, config);
        break;
      default:
        target = null;
    }
    if (target != null) {
      target.setContext(env.ctx);
      target.start();
    }
    var memoryAppender = new MemoryAppender(target);
    env.finalizeAppender(memoryAppender);
    return true;
  }

  @Override
  public boolean setupTelemetryAppender(URI logsEndpoint, boolean logConnectionFailures) {
    LoggerAndContext env = contextInit(Level.DEBUG, config, false);
    AbstractRemoteAppender telemetryAppender;
    try {
      telemetryAppender = AbstractRemoteAppender.loadTelemetryAppender();
      if (telemetryAppender == null) {
        return false;
      }
    } catch (Exception e) {
      return false;
    }
    var rootLogger = env.logger;
    if (rootLogger.getAppender(CONSOLE_APPENDER_NAME) == null) {
      // Console appender must be setup as a fallback first.
      return false;
    }

    telemetryAppender.setName("telemetry");
    telemetryAppender.setEndpoint(logsEndpoint);
    telemetryAppender.setLogConnectionFailures(logConnectionFailures);

    // We set-up a thread 'pool' that will contain at most one thread.
    // If the thread is idle for 60 seconds, it will be shut down.
    var executor = new ThreadPoolExecutor(0, 1, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
    telemetryAppender.setExecutor(executor);

    var telemetryLogger = env.ctx.getLogger(TELEMETRY_ROOT_LOGGER);
    telemetryLogger.addAppender(telemetryAppender);
    telemetryLogger.setLevel(ch.qos.logback.classic.Level.ALL);

    telemetryAppender.setContext(env.ctx);
    telemetryAppender.start();
    return true;
  }

  @Override
  public boolean setupOpenSearchAppender(
      Level logLevel, URI logsEndpoint, boolean logConnectionFailures) {
    LoggerAndContext env = contextInit(logLevel, config, false);
    AbstractRemoteAppender openSearchAppender;
    try {
      openSearchAppender = AbstractRemoteAppender.loadGenericRemoteAppender();
      if (openSearchAppender == null) {
        return false;
      }
    } catch (Exception e) {
      return false;
    }
    openSearchAppender.setName("engine-remote");
    openSearchAppender.setEndpoint(logsEndpoint);
    openSearchAppender.setLogConnectionFailures(logConnectionFailures);

    // We set-up a thread 'pool' that will contain at most one thread.
    // If the thread is idle for 60 seconds, it will be shut down.
    var executor = new ThreadPoolExecutor(0, 1, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
    openSearchAppender.setExecutor(executor);
    var filter =
        new Filter<ILoggingEvent>() {
          @Override
          public FilterReply decide(ILoggingEvent event) {
            var exclude =
                event.getLoggerName().startsWith(TELEMETRY_ROOT_LOGGER)
                    || event.getLoggerName().startsWith("org.enso.logging.service");
            return exclude ? FilterReply.DENY : FilterReply.NEUTRAL;
          }
        };
    filter.setContext(env.ctx);
    filter.start();
    openSearchAppender.addFilter(filter);
    env.finalizeAppender(openSearchAppender);
    return true;
  }

  @Override
  public boolean setupNoOpAppender() {
    LoggerAndContext env = contextInit(Level.ERROR, null, true);

    NOPAppender<ILoggingEvent> appender = new NOPAppender<>();
    appender.setName("enso-noop");

    env.finalizeAppender(appender);
    return true;
  }

  @Override
  public void teardown() {
    context().stop();
    var logLevelOnShutdown =
        config.getLogLevel().map(name -> Level.valueOf(name.toUpperCase())).orElse(Level.ERROR);
    setupConsoleAppender(logLevelOnShutdown);
  }

  private LoggerAndContext contextInit(
      Level level, LoggingServiceConfig config, boolean shouldResetContext) {
    var ctx = context();
    if (shouldResetContext) {
      ctx.reset();
    }
    Logger rootLogger = ctx.getLogger(Logger.ROOT_LOGGER_NAME);

    Filter<ILoggingEvent> filter;
    LoggersLevels loggers = config != null ? config.getLoggers() : null;
    if (loggers != null && !loggers.isEmpty()) {
      filter = ApplicationFilter.fromLoggers(loggers, level, LANG_PREFIX);
    } else {
      filter = null;
    }
    return new LoggerAndContext(level, ctx, rootLogger, filter);
  }

  private record LoggerAndContext(
      Level level, LoggerContext ctx, Logger logger, Filter<ILoggingEvent> filter) {

    void finalizeAppender(ch.qos.logback.core.Appender<ILoggingEvent> appender) {
      if (filter == null) {
        ThresholdFilter threshold = new ThresholdFilter();
        threshold.setLevel(ch.qos.logback.classic.Level.convertAnSLF4JLevel(level).toString());
        appender.addFilter(threshold);
        threshold.setContext(ctx);
        threshold.start();
      }

      // Root's log level is set to the minimal required log level.
      // Log level is controlled by `ThresholdFilter` instead, allowing is to specify different
      // log levels for different outputs.
      var minLevelInt = Math.min(Level.TRACE.toInt(), level.toInt());
      var minLevel =
          ch.qos.logback.classic.Level.convertAnSLF4JLevel(Level.intToLevel(minLevelInt));

      logger.setLevel(minLevel);
      if (filter != null) {
        appender.addFilter(filter);
        filter.setContext(ctx);
        filter.start();
      }
      appender.setContext(ctx);
      appender.start();
      logger.addAppender(appender);
    }
  }

  private static final String LANG_PREFIX = "enso";
}
