---
layout: developer-doc
title: Logging Service
category: infrastructure
tags: [infrastructure, logging, debug]
order: 7
---

# Logging

The Enso project features a centralised logging service to allow for the
aggregation of logs from multiple components. This service can be started with
one of the main components, allowing other components to connect to it. The
service aggregates all logs in one place for easier analysis of the interaction
between components. Components can also log to console or files directly without
involving the centralized logging service. For more information about this
architecture, see [Logging server](#logging-server).

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Configuration](#configuration)
  - [Custom Log Levels](#custom-log-levels)
  - [Appenders](#appenders)
    - [Engine runner](#engine-runner)
    - [Format](#format)
    - [File](#file-appender)
    - [Network](#socket-appender)
- [Logging server](#logging-server)
- [Telemetry](#telemetry)
  - [Logger namespace](#logger-namespace)
  - [LogEvent format](#logevent-format)
  - [Transforming LogEvent to HTTP POST request](#transforming-logevent-to-http-post-request)
  - [Log level](#log-level)
- [JVM Architecture](#jvm-architecture)
  - [SLF4J Interface](#slf4j-interface)
  - [Setting Up Logging](#setting-up-logging)
  - [Log Masking](#log-masking)
  - [Logging in Tests](#logging-in-tests)
  - [Logging to file](#logging-to-file)

<!-- /MarkdownTOC -->

## Configuration

All logging settings are configured via the `logging-service` section of the
`application.conf` config file. Each of the main components can customize format
and output target via section in `application.conf` configuration file. The
configuration is using HOCON-style, as defined by
[lightbend/config](https://github.com/lightbend/config). Individual values
accepted in the config are inspired by SLF4J's properties, formatting and
implementations. Currently 3 components define logging configuration:

- [`launcher`](../../engine/launcher/src/main/resources/application.conf)
- [CLI](../../engine/runner/src/main/resources/application.conf)

The configuration has two main sections:

- [custom log levels](#custom-log-levels)
- [applications' appenders](#appenders) (also known as configuration of log
  events output target)

During component's setup, its `application.conf` config file is parsed. The
config's keys and values are validated and, if correct, the parsed
representation is available as an instance of
`org.enso.logging.config.LoggingServiceConfig` class. The class encapsulates the
`logging-service` section of `application.conf` file and is used to
programmatically initialize loggers.

As per [configuration schema](https://github.com/lightbend/config) any key can
be defined with a default value that can be overridden by an environment
variable. For example

```
  {
    host = localhost
    host = $ENSO_HOST
  }
```

defines a `host` key once, except that `ENSO_HOST` values takes a precedence if
it is defined during loading of the config file.

### Custom Log Levels

Possible log level values are (in the order of precedence):

- `error`
- `warn`
- `info`
- `debug`
- `trace`

The `logging-service.logger` configuration section provides an ability to
override the default application log level for particular loggers. In the
`logger` subconfig the key specifies the logger name (or it's prefix) and the
value specifies the log level for that logger.

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

### Truffle loggers

[TruffleLogger](https://www.graalvm.org/truffle/javadoc/com/oracle/truffle/api/TruffleLogger.html)
is designed to be independent of any other logging facilities. Setting a custom
level for Truffle logger can be done programmatically via
[ContextBuilder.option](<https://www.graalvm.org/truffle/javadoc/org/graalvm/polyglot/Context.Builder.html#option(java.lang.String,java.lang.String)>)
polyglot context builder option, or via
`polyglot.log.<language-id>.<class-name>.level` system property. Note that it is
not enough to set just `-Dpolyglot.log.<language-id>.<class-name>.level` system
property, one has to also specify this level via for forwarded log messages via
`-D<language-id>.<class-name>.Logger.level` prop.

For example, to set trace level for class
`com.oracle.graal.python.runtime.PythonContext` in `python` Truffle language,
use the following properties:

```sh
-Dpolyglot.log.python.com.oracle.graal.python.runtime.PythonContext.level=FINE -Dpython.com.oracle.graal.python.runtime.PythonContext.Logger.level=trace
```

### Appenders

Log output target is configured in the `application.conf` files in the
"appenders" section ("appender" is equivalent to `java.util.logging.Handler`
semantics). Each appender section can provide further required and optional
key/value pairs, to better customize the log target output.

Currently supported are

- console appender - the most basic appender that prints log events to stdout
- [file appender](#file-appender) - appender that writes log events to a file,
  with optional rolling file policy
- [socket appender](#socket-appender) - appender that forwards log events to
  some logging server

The appenders are defined by the `logging-service.appenders`. Currently only a
single appender can be selected at a time, although additional
[logging to file](#logging-to-file) is supported. The selection may also be done
via an environmental variable but it depends on which component we are
executing:

#### Project Manager

Project manager by default starts a centralized logging server that collects
logs (as defined in `logging-service.server` config key) and the logs output can
be overwritten by `ENSO_LOGSERVER_APPENDER` env variable

For example, for the project manager to output to `console` one simply executes

```
ENSO_LOGSERVER_APPENDER=console ./project-manager
```

#### Engine runner

When executing the engine runner component, i.e., CLI usage, via `ensoup` or
`enso`, the default log output can be overwritten by defining the
`ENSO_APPENDER_DEFAULT` env variable

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

In the above example `%logger` format will be substituted with a class name for
which the logger was created with.

By default, console pattern includes `%nopex` formatter which means that any
stacktrace attached to the log message will always be ignored. By default other
appenders do not have such formatting key. This means that if an exception is
included in the logged messaged, a full stacktrace will be attached, if present.

For a full list of formatting keys please refer to the concrete implementation's
[manual](https://logback.qos.ch/manual/layouts.html#ClassicPatternLayout).

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
defaults if missing (defined in `org.enso.logging.config.FileAppender` config
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

## Logging server

The following section describes the _logging server_ architecture when user
opens a project from IDE. In CLI mode (i.e. running `enso --run script.enso`),
there is no _logging server_ - see [Engine runner appender](#engine-runner).

The centralized logging service is implemented as a logging server started by
the
[Project Manager](https://github.com/enso-org/enso/blob/c47dba1d108e0d52e401333d1b6f6f4182436aa7/lib/scala/project-manager/src/main/scala/org/enso/projectmanager/boot/ProjectManager.scala#L326-L345).
The implementation of logging server is in
[org.enso.logging.service.logback.LoggingServer](https://github.com/enso-org/enso/blob/da6a6ae9dde9c0cef02c1d07be5e03660b40d086/lib/scala/logging-service-logback/src/main/java/org/enso/logging/service/logback/LoggingServer.java).
The logging server
[listens](https://github.com/enso-org/enso/blob/da6a6ae9dde9c0cef02c1d07be5e03660b40d086/lib/scala/logging-service-logback/src/main/java/org/enso/logging/service/logback/SocketServer.java#L56)
to clients that connect via a socket. The client is, for example, language
server that is started as a subprocess by the project manager. The clients use
[org.enso.logging.service.logback.DeferredProcessingSocketAppender](https://github.com/enso-org/enso/blob/566d3d503e35ccf8c363facd590d1332683bce3a/lib/scala/logging-service-logback/src/main/java/org/enso/logging/service/logback/DeferredProcessingSocketAppender.java)
to send log events to the logging server. The logging server then properly
dispatches the received logging event to all the appenders in
[SocketLoggingNode](https://github.com/enso-org/enso/blob/3e0b4dd7413e45373dc4ac26f124ee6aedffd50c/lib/scala/logging-service-logback/src/main/java/org/enso/logging/service/logback/SocketLoggingNode.java#L87-L94).

## Telemetry

Telemetry gathers anonymized, yet still useful information about the environment
Enso IDE operates at. Such metadata (not user data) are send to the Enso cloud
to improve planning and further help to optimize Enso user experience.

Telemetry events are just logging messages in a special format. Using logging
infrastructure makes it easy to collect telemetry from all possible sources -
engine, standard libraries, language server and project manager. All the
telemetry events are visible in our OpenSearch dashboard. To properly send
telemetry logs to the cloud, the used logger and the log message must conform to
the specification described below.

Note that the telemetry is collected only when running Enso via
[Project Manager](#project-manager). In CLI mode, when running via
[Engine runner](#engine-runner), no telemetry is collected.

### Logger namespace

To send telemetry data to our cloud endpoint, a logger inside
`org.enso.telemetry` namespace must be used. To create such a logger from Java,
use something like this:

```java
var logger = org.slf4j.LoggerFactory.getLogger("org.enso.telemetry.MyLogger");
```

To create this logger in Enso, use:

```
polyglot java import org.slf4j.LoggerFactory
logger = LoggerFactory.getLogger "org.enso.telemetry.MyLogger"
```

### LogEvent format

The format of the message passed to the logger is important. If it does not
follow this specification, it will not be forwarded to the cloud endpoint. The
message consist of two parts: `message` and `arguments` delimited by a colon:
`<message>: <arguments>`. The `message` part is a string that describes the
event. The `arguments` part is a mapping of `name={}` pairs separated by a
comma. Note that for the compatibility with standard `ch.qos.logback` appenders,
it is important that the value of argument is `{}`. The actual argument values
are passed to the message as argument array via
`ILoggingEvent.getArgumentArray()` method. Number of these arguments must match
number of the arguments given in the message.

An example in Java is:

```java
logger.trace("MyEvent: name={}, age={}", "John", 42);
```

In Enso, one has to use
[org.slf4j.spi.LoggingEventBuilder](https://github.com/qos-ch/slf4j/blob/master/slf4j-api/src/main/java/org/slf4j/spi/LoggingEventBuilder.java)
like this:

```
event_bldr = logger.atTrace
event_bldr.setMessage "MyEvent: name={}, age={}"
event_bldr.addArgument "John"
event_bldr.addArgument 42
event_bldr.log
```

The reason for that is that the `trace` method accepts vararg and there is no
way to pass a vararg arguments from Enso to Java. Note that this is a Truffle
API limitation, not something that could be fixed on Enso side.

### Transforming LogEvent to HTTP POST request

If the LogEvent has the correct aforementioned format, it is delegated to the
`TelemetryAppender` that is responsible for sending HTTP POST requests
asynchronously to the cloud endpoint. The payload of the message looks roughly
like this:

```json
{
  "message": "MyEvent",
  "metadata": {
    "loggerName": "org.enso.telemetry.MyLogger",
    "name": "John",
    "age": 42
  }
}
```

The actual format expected by the cloud is defined at
[logs/endpoints/remote.rs](https://github.com/enso-org/cloud-v2/blob/main/src/lambdas/src/lambdas/logs/endpoints/remote.rs#L169).

If the format of the LogEvent violates the specification, a warning will be ...
warning in the logs, and nothing will be sent.

### Log level

TelemetryAppender is enabled by default for all logging levels. If you wish to
send telemetry event only to the TelemetryAppender, use `TRACE` level. If you
use `DEBUG` level, it will, by default, be also send to the FileAppender. If you
use `INFO` level, it will, by default, be also send to the FileAppender and
ConsoleAppender.

Note that changing the log level for the `org.enso.telemetry` namespace, either
via `application.conf` or via system property, will not affect the Telemetry
Appender. The Telemetry Appender is always enabled for all log levels.

## JVM Architecture

Enso's logging makes use of two logging APIs - `java.util.logging` and
`org.slf4j`. The former is being used by the Truffle runtime, which itself
relies on `jul`, while the latter is used everywhere else. The implementation of
the logging is using off the shelf `Logback` implementation with some custom
setup methods. The two APIss cooperate by essentially forwarding log messages
from the former to the latter.

While typically any SLF4J customization would be performed via custom
`LoggerFactory` and `Logger` implementation that is returned via a
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
setting up logging is `org.enso.logging.config.LoggerSetup`. An instance of that
class can be retrieved with the thread-safe
`org.enso.logging.config.LoggerSetup.get` factory method.
`org.enso.logging.config.LoggerSetup` provides a number of `setupXYZAppender`
methods that will direct loggers to send log events to an `XYZ` appender.
Setting a specific hard-coded appender programmatically should however be
avoided by the users. Instead, one should invoke one of the overloaded `setup`
variants that initialize loggers based on the provided `logging-service`
configuration.

```java
package foo;
import org.enso.logging.config.LoggerSetup;
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

`org.enso.logging.service.LoggingSetupHelper` class was introduced to help with
the most common use cases - establishing a file-based logging in the Enso's
dedicated directories or connecting to an existing logging server once it starts
accepting connections. That is why services don't call `LoggerSetup` directly
but instead provide a service-specific implementation of
`org.enso.logging.service.LoggingSetupHelper`. `LoggingSetupHelper` and
`LoggerSetup` provide `teardown` methods to properly dispose of log events.

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
file. This means that even though a user might be shown only `WARNING` level
logs in the console, logs with up to `DEBUG` or `TRACE` level, including full
stacktraces, can be dumped into the log file. A user can disable this parallel
logging to a file by setting the environment variable:

```
ENSO_LOG_TO_FILE=false project-manager ...
```

Users can fully control the maximal log level used when logging to a log file by
setting the environment variable:

```
ENSO_LOG_TO_FILE_LOG_LEVEL=trace project-manager ...
```

For example, in the above example `project-manager` will log events of up-to
`trace` in the log file.

**Note** Logging to a file requires presence of the `file`
[appender](#file-appender) in the `logging-service.appenders` section.

# How to use logging

Logging infrastructure uses a popular SLF4J interface which most of developers
should be familiar with. In this section we include a only small number of
examples, full user manual is available at SLF4J's
[website](https://www.slf4j.org/manual.html).

## Log a simple INFO message

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HelloWorld {

  public static void main(String[] args) {
    Logger logger = LoggerFactory.getLogger(HelloWorld.class);
    logger.info("Hello World");
  }
}
```

## Log a simple INFO message only if TRACE is enabled

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HelloWorld {

  public static void main(String[] args) {
    Logger logger = LoggerFactory.getLogger(HelloWorld.class);
    if (logger.isTraceEnabled()) {
      logger.info("Hello World");
    }
  }
}
```

## Log an exception

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HelloWorld {

  public static void main(String[] args) {
    Logger logger = LoggerFactory.getLogger(HelloWorld.class);
    Throwable ex = new RuntimeException("foo");
    logger.error("Hello World", ex);
  }
}
```

Note that in order for the full stacktrace to be printed, pattern in the desired
appender must not contain `%nopex` formatting key. See [formatting](#format) for
details.

## Log a telemetry event

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HelloWorld {

  public static void main(String[] args) {
    Logger logger = LoggerFactory.getLogger("org.enso.telemetry.My.SuperCool.TelemetryLogger");
    logger.trace("This is a telemetry message: arg1={}, arg2={}", "foo", 42);
  }
}
```

See [Telemetry](#telemetry) section for more details.
