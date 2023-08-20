package org.enso.logger;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.net.SocketAppender;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.classic.encoder.PatternLayoutEncoder;
import ch.qos.logback.core.FileAppender;
import ch.qos.logback.core.ConsoleAppender;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.helpers.NOPAppender;

import java.io.File;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import org.enso.logger.config.LoggingServiceConfig;
import org.enso.logger.config.Appender;
import org.enso.logger.config.Loggers;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;

public class LoggerSetup {

    private static final String defaultPattern = "[%level] [%d{yyyy-MM-ddTHH:mm:ssXXX}] [%logger] %msg%n";

    private static final String fileAppenderKey = "file";
    private static final String socketAppenderKey = "socket";
    private static final String consoleAppenderKey = "console";

    public static Boolean setup() {
        LoggingServiceConfig config = LoggingServiceConfig.parseConfig();
        return setup(config);
    }

    public static Boolean setup(LoggingServiceConfig config) {
        Level defaultLogLevel = config.getLogLevel().map(name -> Level.valueOf(name.toUpperCase())).orElseGet(() -> Level.ERROR);
        return setup(defaultLogLevel, config);
    }

    public static Boolean setup(Level logLevel) {
        return setup(logLevel, LoggingServiceConfig.parseConfig());
    }

    public static Boolean setup(Level logLevel, LoggingServiceConfig config) {
        Appender defaultAppender = config.getAppender();
        if (defaultAppender != null) {
            switch (defaultAppender.getName()) {
                case fileAppenderKey:
                    return setupFileAppender(logLevel, null, null, config);
                case socketAppenderKey:
                    return setupSocketAppender(logLevel, defaultAppender.getConfig().getString("hostname"), defaultAppender.getConfig().getInt("port"), config);
                default:
                    return setupConsoleAppender(logLevel, config);
            }
        } else {
            return setupConsoleAppender(logLevel, config);
        }
    }
    public static Boolean setupSocketAppender(
            Level logLevel,
            String hostname,
            int port,
            LoggingServiceConfig config) {
        LoggerAndContext env = contextInit(logLevel, config);

        SocketAppender socketAppender = new SocketAppender();
        socketAppender.setName("enso-socket");
        socketAppender.setIncludeCallerData(true);
        socketAppender.setRemoteHost(hostname);
        socketAppender.setPort(port);

        env.finalizeAppender(socketAppender);

        return true;
    }


    public static Boolean setupFileAppender(
            Level logLevel,
            Path logRoot,
            String logPrefix,
            LoggingServiceConfig config
    ) {
        LoggerAndContext env = contextInit(logLevel, config);

        String configPattern = ((org.enso.logger.config.ConsoleAppender)config.getAppender(fileAppenderKey)).getPattern();
        final PatternLayoutEncoder encoder = new PatternLayoutEncoder();
        encoder.setPattern(configPattern == null ? defaultPattern : configPattern);
        env.finalizeEncoder(encoder);

        FileAppender<ILoggingEvent> fileAppender = new FileAppender<>();
        fileAppender.setName("enso-file");
        fileAppender.setAppend(true);
        fileAppender.setImmediateFlush(true);
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        String currentDate = LocalDate.now().format(dtf);
        String fullFilePath;
        if (logRoot == null || logPrefix == null) {
            fullFilePath = "enso-" + currentDate + ".log";
        } else {
            fullFilePath = logRoot.toAbsolutePath() + File.separator + logPrefix + "-" + currentDate + ".log";
        }
        fileAppender.setEncoder(encoder);
        fileAppender.setFile(fullFilePath);

        env.finalizeAppender(fileAppender);
        return true;
    }

    public static Boolean setupConsoleAppender(Level logLevel, LoggingServiceConfig config) {
        LoggerAndContext env = contextInit(logLevel, config);

        String consolePattern;
        if (config != null) {
            consolePattern = ((org.enso.logger.config.ConsoleAppender)config.getAppender(consoleAppenderKey)).getPattern();
        } else {
            consolePattern = null;
        }
        final PatternLayoutEncoder encoder = new PatternLayoutEncoder();
        encoder.setPattern(consolePattern == null ? defaultPattern : consolePattern);
        env.finalizeEncoder(encoder);

        ConsoleAppender<ILoggingEvent> consoleAppender = new ConsoleAppender<>();
        consoleAppender.setName("enso-console");
        consoleAppender.setEncoder(encoder);

        env.finalizeAppender(consoleAppender);
        return true;
    }

    public static Boolean setupNoOpAppender() {
        LoggerAndContext env = contextInit(Level.ERROR, null);

        NOPAppender<ILoggingEvent> appender = new NOPAppender<>();
        appender.setName("enso-noop");

        env.finalizeAppender(appender);
        return true;
    }

    public static void teardown() {
        // TODO: disable whatever appender is now in place and replace it with console
        var context = (LoggerContext) LoggerFactory.getILoggerFactory();
        context.stop();
    }

    private static  LoggerAndContext contextInit(Level level, LoggingServiceConfig config) {
        LoggerContext context = (LoggerContext) LoggerFactory.getILoggerFactory();
        context.reset();
        context.setName("enso-custom");
        Logger rootLogger = context.getLogger(Logger.ROOT_LOGGER_NAME);

        Filter<ILoggingEvent> filter;
        Loggers loggers = config != null ? config.getLoggers() : null;
        if (loggers != null) {
            filter = ApplicationFilter.fromLoggers(loggers);
        } else {
            filter = null;
        }
        return new LoggerAndContext(level, context, rootLogger, filter);
    }

    private record LoggerAndContext(Level level, LoggerContext ctx, Logger logger, Filter<ILoggingEvent> filter) {

        void finalizeEncoder(ch.qos.logback.core.encoder.Encoder<ILoggingEvent> encoder) {
            encoder.setContext(ctx);
            encoder.start();
        }
        void finalizeAppender(ch.qos.logback.core.Appender<ILoggingEvent> appender) {
            logger.setLevel(ch.qos.logback.classic.Level.convertAnSLF4JLevel(level));
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
