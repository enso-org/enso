---
layout: developer-doc
title: Logging Service
category: infrastructure
tags: [infrastructure, logging, debug]
order: 6
---

# Logging

The Enso project features a centralised logging service to allow for the
aggregation of logs from multiple components. This service can be started with
one of the main components, allowing other components to connect to it. The
service aggregates all logs in one place for easier analysis of the interaction
between components. Components can also log to console or files directly without
involving the centralized logging service.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Configuration](#configuration)
  - [Custom Log Levels](#custom-log-levels)
  - [Appenders](#appenders)
    - [Format](#format)
    - [File](#file-appender)
    - [Network](#socket-appender)
    - [Sentry.io](#sentry-appender)
- [JVM Architecture](#jvm-architecture)
  - [SLF4J Interface](#slf4j-interface)
  - [Setting Up Logging](#setting-up-logging)
  - [Log Masking](#log-masking)
  - [Logging in Tests](#logging-in-tests)
  - [Logging to file](#logging-to-file)

<!-- /MarkdownTOC -->

## Configuration

The logging settings should be placed under the `logging-service` key of the
`application.conf` config. Each of the main components can customize format and
output target via section in `application.conf` configuration file. The
configuration is using HOCON-style, as defined by
[lightbend/config](https://github.com/lightbend/config). Individual values
accepted in the config are inspired by SLF4J's properties, formatting and
implementations.

The configuration has two main sections:

- [custom log levels](#custom-log-levels)
- [applications' appenders](#appenders) (also known as configuration of log
  events output target)

During component's setup, its `application.conf` config file is parsed. The
config's keys and values are validated and, if correct, the parsed
representation is available as an instance of
`org.enso.logger.config.LoggingServiceConfig` class. The class encapsulates the
`logging-service` section of `application.conf` file and is used to
programmatically initialize loggers.

As per [configuration schema](https://github.com/lightbend/config) any key can
have a default value that can be overridden by an environment variable. For
example

```
  {
    host = localhost
    host = $ENSO_HOST
  }
```

defines a `host` key once, except that `ENSO_HOST` values takes a precedence if
it is defined during loading of the config file.

### Custom Log Levels

The `logging-service.logger` configuration provides an ability to override the
default application log level for particular loggers. In the `logger` subconfig
the key specifies the logger name (or it's prefix) and the value specifies the
log level for that logger.

```
logging-service.logger {
  akka.actor = info
  akka.event = error
  akka.io = error
  slick {
    jdbc.JdbcBackend.statement = debug
    "*" = error
  }
}
```

For example, the config above limits all `akka.actor.*` loggers to the info
level logging, and `akka.event.*` loggers can emit only the error level
messages.

Config supports globs (`*`). For example, the config above sets
`jdbc.JdbcBackend.statement` SQL statements logging to debug level, and the rest
of the slick loggers to error level.

Additionally, custom log events can be provided during runtime via system
properties, without re-packaging the updated config file. For example

```typescript
akka.actor = info;
```

is equivalent to

```typescript
  -Dakka.actor.Logger.level=info
```

Any custom log level is therefore defined with `-Dx.y.Z.Logger.level` where `x`,
`y` and `Z` refer to the package elements and class name, respectively. System
properties always have a higher priority over those defined in the
`application.conf` file.

### Appenders

Log output target is also configured in the `application.conf` files in the
"appenders" section ("appender" is equivalent to `java.util.logging.Handler`
semantics). Each appender section can provide further required and optional
key/value pairs, to better customize the log target output.

Currently supported are

- console appender - the most basic appender that prints log events to stdout
- [file appender](#file-appender) - appender that writes log events to a file,
  with optional rolling file policy
- [socket appender](#socket-appender) - appender that forwards log events to
  some logging server
- [sentry.io appender](#sentry-appender) - appender that forwards log events to
  a sentry.io service

The appenders are defined by the `logging-service.appenders`. Currently only a
single appender can be selected at a time. The selection may also be done via an
environmental variable but it depends on which component we are executing.

- `project-manager` - project manager by default starts a centralized logging
  server that collects logs (as defined in `logging-service.server` config key)
  and the logs output can be overwritten by `ENSO_LOGSERVER_APPENDER` env
  variable
- `launcher` or `runner` - the default log output can be overwritten by defining
  the `ENSO_APPENDER_DEFAULT` env variable

For example, for the project manager to output to `console` one simply executes

```
ENSO_LOGSERVER_APPENDER=console ./project-manager
```

#### Format

The pattern follows the classic's
[PatternLayout](https://logback.qos.ch/manual/layouts.html#ClassicPatternLayout)
format.

Appenders that store/display log events can specify the format of the log
message via `pattern` field e.g.

```typescript

  appenders = [
    {
      name = "console"
      pattern = "[%level{lowercase=true}] [%d{yyyy-MM-dd'T'HH:mm:ssXXX}] [%logger] %msg%n%nopex"
    }
    ...
  ]
```

#### File Appender

Enabled with `ENSO_APPENDER_DEFAULT=file` environment variable.

File appender directs all log events to a log file:

```
  {
    name = "file"
    append = <boolean, optional>
    immediate-flush = <boolean, optional>
    pattern = <string, optional>
    rolling-policy {
      max-file-size = <string, optional>
      max-history = <int, optional>
      max-total-size = <string, optional>
    }
  }
```

Rolling policy is a fully optional property of File Appender that would trigger
automatic log rotation. All properties are optional with some reasonable
defaults if missing (defined in `org.enso.logger.config.FileAppender` config
class).

#### Socket Appender

Enabled with `ENSO_APPENDER_DEFAULT=socket` environment variable.

Configuration

```
  {
    name = "socket"
    hostname = <string, required>
    port = <string, required>
  }
```

The two fields can be overridden via environment variables:

- `hostname` has an equivalent `$ENSO_LOGSERVER_HOSTNAME` variable
- `port` has an equivalent `$ENSO_LOGSERVER_PORT` variable

#### Sentry Appender

Enabled with `ENSO_APPENDER_DEFAULT=sentry` environment variable.

```
  {
    name = "sentry"
    dsn = <string, required>
    flush-timeout = <int, optional>
    debug = <boolean, optional>
  }
```

Sentry's Appender has a single required field, `dsn`. The `dsn` value can be
provided via an environment variable `ENSO_APPENDER_SENTRY_DSN`. `flush-timeout`
determines how often logger should send its collected events to sentry.io
service. If `debug` value is `true`, logging will print to stdout additional
trace information of the logging process itself.

## JVM Architecture

Enso's logging makes use of two logging APIs - `java.util.logging` and
`org.slf4j`. The former is being used Truffle runtime, which itself relies on
`jul`, while the latter is used everywhere else. The implementation of the
logging is using off the shelf `Logback` implementation with some custom setup
methods. The two APIss cooperate by essentially forwarding log messages from the
former to the latter.

While typically any SLF4J customization would be performed via custom
`LoggerFacotry` and `Logger` implementation that is returned via a
`StaticLoggerBinder` instance, this is not possible for our use-case:

- file logging requires Enso-specific directory which is only known during
  runtime
- centralized logging
- modifying log levels without recompilation

### SLF4J Interface

The user code must not be calling any of the underlying implementations, such as
Log4J or Logback, and should only request loggers via factory methods.

One can use the `org.slf4j.LoggerFactory` directly to retrieve class-specific
logger. For Scala code, it is recommended to use the
`com.typesafe.scalalogging.Logger` instead which wraps the SLF4J logger with
macros that compute the log messages only if the given logging level is enabled,
and allows much prettier initialisation.

```java
package foo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Foo {
    private Logger logger = LoggerFactory.getLogger(Foo.class);

    public void bar() {
        logger.info("Hello world!");
    }
}
```

### Setting Up Logging

The `org.slf4j.Logger` instances have to know where to send log events. This
setting is typically performed once, when the service starts, and applies
globally during its execution. Currently, it is not possible to dynamically
change where log events are being stored. The main (abstract) class used for
setting up logging is `org.enso.logger.LoggerSetup`. An instance of that class
can be retrieved with the thread-safe `org.enso.logger.LoggerSetup.get` factory
method. `org.enso.logger.LoggerSetup` provides a number of `setupXYZAppender`
methods that will direct loggers to send log events to an `XYZ` appender.
Setting a specific hard-coded appender programmatically should however be
avoided by the users. Instead, one should invoke one of the overloaded `setup`
variants that initialize loggers based on the provided `logging-service`
configuration.

```java
package foo;
import org.enso.logger.LoggerSetup;
import org.slf4j.event.Level;

public class MyService {

  private Logger logger = LoggerFactory.getLogger(Foo.class);
  ...
  public void start(Level logLevel) {
    LoggerSetup.get().setup(logLevel);
    logger.info("My service is starting...");
    ...
  }
  ...
}
```

`org.enso.logging.LoggingSetupHelper` class was introduced to help with the most
common use cases - establishing a file-based logging in the Enso's dedicated
directories or connecting to an existing logging server once it starts accepting
connections. That is why services don't call `LoggerSetup` directly but instead
provide a service-specific implementation of
`org.enso.logging.LoggingSetupHelper`. `LoggingSetupHelper` and `LoggerSetup`
provide `teardown` methods to properly dispose of log events.

### Log Masking

Logs should not contain personally identifiable information (PII). The following
is considered PII:

- User code
- Values of executed expressions
- Values of user environment variables. Although variable names are not
  considered PII and can be logged.
- File paths inside the user project directory. System and distribution paths
  and a path to the user project can be logged.

Project logging library implements masking of PII. To utilize it

1. Logged object should implement an interface that defines custom log-string
   representation of this object
2. The logging should be performed by supplying a template string with `{}`
   placeholders, and the arguments
   ```scala
   log.debug("Created {} at [{}].", obj, path)
   ```

String interpolation in log statements `s"Created $obj"` should be avoided
because it uses default `toString` implementation and can leak critical
information even if the object implements custom interface for masked logging.

### Logging in Tests

The Logging Service provides a helper function `TestLogger.gatherLogs` that will
execute the closure and collect all logs reported in the specified class. That
way it can verify that all logs are being reported within the provided code.

### Logging to file

By default Enso will attempt to persist (verbose) logs into a designated log
file. This means that even though a user might be shown `WARNING` level logs in
the console, Logs with up to `TRACE` level will be dumped into the log file. A
user can disable this parallel logging to a file by setting the environment
variable:

```
ENSO_LOG_TO_FILE=false project-manager ...
```
