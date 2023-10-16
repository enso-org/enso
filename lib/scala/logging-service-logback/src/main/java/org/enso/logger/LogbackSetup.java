package org.enso.logger;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.filter.ThresholdFilter;
import ch.qos.logback.classic.net.SocketAppender;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import ch.qos.logback.core.FileAppender;
import ch.qos.logback.core.ConsoleAppender;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.helpers.NOPAppender;

import ch.qos.logback.core.rolling.RollingFileAppender;
import ch.qos.logback.core.rolling.SizeAndTimeBasedRollingPolicy;
import ch.qos.logback.core.util.Duration;
import ch.qos.logback.core.util.FileSize;
import io.sentry.SentryLevel;
import io.sentry.SentryOptions;
import io.sentry.SystemOutLogger;
import io.sentry.logback.SentryAppender;

import java.io.File;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import org.enso.logger.config.*;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;

@org.openide.util.lookup.ServiceProvider(service = LoggerSetup.class)
public final class LogbackSetup extends LoggerSetup {

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
        Level defaultLogLevel = config.getLogLevel().map(name -> Level.valueOf(name.toUpperCase())).orElseGet(() -> Level.ERROR);
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
    public boolean setup(Level logLevel, Path componentLogPath, String componentLogPrefix, LoggingServiceConfig config) {
        Appender defaultAppender = config.getAppender();
        if (defaultAppender != null) {
            return defaultAppender.setupForPath(logLevel, componentLogPath, componentLogPrefix, this);
        } else {
            return setupConsoleAppender(logLevel);
        }
    }

    @Override
    public boolean setupSocketAppender(
            Level logLevel,
            String hostname,
            int port) {
        Level targetLogLevel;
        // Modify log level if we were asked to always log to a file.
        // The receiver needs to get all logs (up to `trace`) so as to be able to log all verbose messages.
        if (logToFileEnabled()) {
            int min = Math.min(Level.TRACE.toInt(), config.logToFile().logLevel().toInt());
            targetLogLevel = Level.intToLevel(min);
        } else {
            targetLogLevel = logLevel;
        }
        LoggerAndContext env = contextInit(targetLogLevel, config, !logToFileEnabled());

        org.enso.logger.config.SocketAppender appenderConfig = config.getSocketAppender();

        SocketAppender socketAppender = new SocketAppender();
        socketAppender.setName("enso-socket");
        socketAppender.setIncludeCallerData(false);
        socketAppender.setRemoteHost(hostname);
        socketAppender.setPort(port);
        if (appenderConfig != null)
            socketAppender.setReconnectionDelay(Duration.buildByMilliseconds(appenderConfig.getReconnectionDelay()));

        env.finalizeAppender(socketAppender);
        return true;
    }


    @Override
    public boolean setupFileAppender(
            Level logLevel,
            Path logRoot,
            String logPrefix) {
        try {
            LoggerAndContext env = contextInit(logLevel, config, true);
            org.enso.logger.config.FileAppender appenderConfig = config.getFileAppender();
            if (appenderConfig == null) {
                throw new MissingConfigurationField(org.enso.logger.config.FileAppender.appenderName);
            }
            final PatternLayoutEncoder encoder = new PatternLayoutEncoder();
            encoder.setPattern(appenderConfig.getPattern());
            env.finalizeEncoder(encoder);

            FileAppender<ILoggingEvent> fileAppender;


            if (appenderConfig != null && appenderConfig.getRollingPolicy() != null) {
                RollingFileAppender<ILoggingEvent> rollingFileAppender = new RollingFileAppender<>();
                fileAppender = rollingFileAppender;
                fileAppender.setContext(env.ctx); // Context needs to be set prior to rolling policy initialization
                String filePattern;
                if (logRoot == null || logPrefix == null) {
                    filePattern = "enso-%d{yyyy-MM-dd}";
                } else {
                    filePattern = logRoot.toAbsolutePath() + File.separator + logPrefix + "-" + "%d{yyyy-MM-dd}";
                }

                org.enso.logger.config.FileAppender.RollingPolicy rollingPolicy = appenderConfig.getRollingPolicy();
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
                    fullFilePath = logRoot.toAbsolutePath() + File.separator + logPrefix + "-" + currentDate + ".log";
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
        org.enso.logger.config.ConsoleAppender appenderConfig = config.getConsoleAppender();
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
        env.finalizeEncoder(encoder);

        ConsoleAppender<ILoggingEvent> consoleAppender = new ConsoleAppender<>();
        consoleAppender.setName("enso-console");
        consoleAppender.setEncoder(encoder);

        env.finalizeAppender(consoleAppender);
        return true;
    }

    @Override
    public boolean setupSentryAppender(Level logLevel, Path logRoot) {
        // TODO: handle proxy
        // TODO: shutdown timeout configuration
        try {
            LoggerAndContext env = contextInit(logLevel, config, !logToFileEnabled());

            org.enso.logger.config.SentryAppender appenderConfig = config.getSentryAppender();
            if (appenderConfig == null) {
                throw new MissingConfigurationField(org.enso.logger.config.SentryAppender.appenderName);
            }
            SentryAppender appender = new SentryAppender();
            SentryOptions opts = new SentryOptions();
            if (appenderConfig.isDebugEnabled()) {
                opts.setDebug(true);
                opts.setLogger(new SystemOutLogger());
                opts.setDiagnosticLevel(SentryLevel.ERROR);
            }
            if (logRoot == null) {
                opts.setCacheDirPath("sentry");
            } else {
                opts.setCacheDirPath(logRoot.resolve(".sentry").toAbsolutePath().toString());
            }
            if (appenderConfig.getFlushTimeoutMs() != null) {
                opts.setFlushTimeoutMillis(appenderConfig.getFlushTimeoutMs());
            }
            appender.setMinimumEventLevel(ch.qos.logback.classic.Level.convertAnSLF4JLevel(logLevel));
            opts.setDsn(appenderConfig.getDsn());
            appender.setOptions(opts);

            env.finalizeAppender(appender);
        } catch (Throwable e) {
            e.printStackTrace();
            return false;
        }
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
        // TODO: disable whatever appender is now in place and replace it with console
        context().stop();
    }

    private LoggerAndContext contextInit(Level level, LoggingServiceConfig config, boolean shouldResetContext) {
        var ctx = context();
        if (shouldResetContext) {
            ctx.reset();
        }
        Logger rootLogger = ctx.getLogger(Logger.ROOT_LOGGER_NAME);

        Filter<ILoggingEvent> filter;
        LoggersLevels loggers = config != null ? config.getLoggers() : null;
        if (loggers != null && !loggers.isEmpty()) {
            filter = ApplicationFilter.fromLoggers(loggers);
        } else {
            filter = null;
        }
        return new LoggerAndContext(level, ctx, rootLogger, filter);
    }

    private record LoggerAndContext(Level level, LoggerContext ctx, Logger logger, Filter<ILoggingEvent> filter) {

        void finalizeEncoder(ch.qos.logback.core.encoder.Encoder<ILoggingEvent> encoder) {
            encoder.setContext(ctx);
            encoder.start();
        }
        void finalizeAppender(ch.qos.logback.core.Appender<ILoggingEvent> appender) {
            ThresholdFilter threshold = new ThresholdFilter();
            threshold.setLevel(ch.qos.logback.classic.Level.convertAnSLF4JLevel(level).toString());
            appender.addFilter(threshold);
            threshold.setContext(ctx);
            threshold.start();

            // Root's log level is set to the minimal required log level.
            // Log level is controlled by `ThresholdFilter` instead, allowing is to specify different
            // log levels for different outputs.
            var minLevelInt = Math.min(Level.TRACE.toInt(), level.toInt());
            var minLevel = ch.qos.logback.classic.Level.convertAnSLF4JLevel(Level.intToLevel(minLevelInt));

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
}
