---
layout: developer-doc
title: Logging Service
category: infrastructure
tags: [infrastructure, logging, debug]
order: 6
---

# Logging

A centralized service has been created to better manage logging from multiple
components. This service can be started with one of the main components,
allowing other components connect to it. All logs are gathered in one place for
easier analysis of component's interaction.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Protocol](#protocol)
  - [Message Examples](#message-examples)
- [JVM Architecture](#jvm-architecture)
  - [SLF4J Interface](#slf4j-interface)
  - [Setting Up Logging](#setting-up-logging)

<!-- /MarkdownTOC -->

## Protocol

The service is designed to be usable by a variety of components. It relies on a
WebSocket connection to a specified endpoint that exchanges JSON-encoded text
messages. The communication is uni-directional - the only messages are log
messages that are sent from a connected client to the server that gathers all
logs.

Each message is encoded as a JSON object with the following fields:

- `level` - an integer between 0 and 4 that indicates the log level:
  - 0 - `ERROR`
  - 1 - `WARN`
  - 2 - `INFO`
  - 3 - `DEBUG`
  - 4 - `TRACE`
- `time` - an integer representing the message timestamp in UTC encoded as
  milliseconds since the EPOCH
- `group` - an identifier of log group - the group should indicate which
  component the message originated from and any (possibly nested) context the
  message is associated with; the parts of the group expression are separated
  with `.`
- `message` - the log message
- (optional) `exception` - an exception with a stack-trace associated with the
  message; this field can be not defined or be `null` to indicate lack of an
  exception, or contain an encoded exception, as described below.

The optional exceptions are JSON objects with the following fields:

- `name` - name of the exception, in Java this can be the qualified classname
- `message` - message contained within the exception (may be empty)
- `trace` - a list of trace elements (may be empty), each trace element is an
  object with two fields:
  - `element` - 'name' of the trace element, for example in Java this can be a
    qualified method name
  - `location` - related location, usually consists of a filename and line
    number, for example in Java it can be `Example.java:123` or in Enso
    `Example.enso:4:3-19`
- (optional) `cause` - if the field is defined and is not `null`, it represents
  another instance of an encoded exception that is a cause of this one.

### Message Examples

For example, an error message with an attached exception may look like this (the
class names are made up):

```json
{
  "level": 0,
  "time": 1600864353151,
  "group": "org.enso.launcher.Main",
  "message": "Failed to load a configuration file.",
  "exception": {
    "name": "org.enso.launcher.config.ConfigurationLoaderFailure",
    "message": "Configuration file does not exist.",
    "trace": [
      {
        "element": "org.enso.launcher.config.ConfigurationLoader.load",
        "location": "ConfigurationLoader.scala:123"
      },
      {
        "element": "org.enso.launcher.Main",
        "location": "Main.scala:42"
      }
    ],
    "cause": {
      "name": "java.io.FileNotFoundException",
      "message": "config.yaml (No such file or directory)",
      "trace": []
    }
  }
}
```

Another example could be an info message (without attached exceptions):

```json
{
  "level": 2,
  "time": 1600864353151,
  "group": "org.enso.launcher.Main",
  "message": "Configuration file loaded successfully."
}
```

## JVM Architecture

A default implementation of both a client and server for the logger service are
provided for the JVM.

### SLF4J Interface

The `logging-service` provides a class `org.enso.loggingservice.WSLogger` which
implements the `org.slf4j.Logger` interface, so it is compatible with all code
using SLF4J logging. When the `logging-service` is added to a project, it
automatically binds its logger instance as the SLF4J backend. So from the
perspective of the user, all that they have to do is use SLF4J compliant logging
in the application.

One can use the `org.slf4j.LoggerFactory` directly, but for Scala code, it is
much better to use the `com.typesafe.scalalogging.Logger` which wraps the SLF4J
logger with macros that compute the log messages only if the given logging level
is enabled, and allows much prettier initialisation. Additionally, the
`logging-service` provides a syntactic sugar for entering nested contexts.

```
package foo
import com.typesafe.scalalogging.Logger
import org.enso.logger.LoggerSyntax

class Foo {
  private val logger = Logger[classOf[Foo]]

  def bar(): Unit = {
    logger.info("Hello world") // Logs `Hello world` from context `foo.Foo`.
    baz()
  }

  def baz(): Unit = {
    val bazLogger = logger.enter("baz")
    bazLogger.warn("Inner") // Logs `Inner` from context `foo.Foo.baz`
  }
}
```

### Setting Up Logging

The logger described above must know where it should send its logs, this is
handled by the `WSLoggerManager`. It allows to configure the logging location,
log level and setup the logging service for three modes:

- _Server mode_, that will listen on a given port, gather both local and remote
  logs and print them to stderr and to a file.
- _Client mode_, that will connect to a specified server and send all of its
  logs there. It will not print anything.
- _Fallback mode_, that will just write the logs to stderr (and optionally) a
  file, without setting up any services or connections.

This logging mode initialization cannot usually happen at the time of static
initialization, since the connection details may depend on CLI arguments or
other configuration which may not be accessed immediately. To help with this,
the logger will buffer any log messages that are issued before the
initialization has happened and send them as soon as the service is initialized.

In a rare situation where the service would not be initialized for a long time,
after exceeding some set message limit, the service will automatically set to
_Fallback mode_ and print the buffered messages to avoid storing too many of
them. If it is properly initialized later, these messages will be 'lost', but
any new messages will be redirected to the proper service.

> TODO: if possible we may also try adding a JVM shutdown hook that will check
> if the logger was initialized and if it wasn't, dump all queued messages
> before shutdown
