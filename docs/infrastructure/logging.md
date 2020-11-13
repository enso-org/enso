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
one of the main components, allowing other components connect to it. The service
aggregates all logs in one place for easier analysis of the interaction between
components.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Protocol](#protocol)
  - [Types](#types)
  - [Messages](#messages)
  - [Examples](#examples)
- [JVM Architecture](#jvm-architecture)
  - [SLF4J Interface](#slf4j-interface)
  - [Setting Up Logging](#setting-up-logging)

<!-- /MarkdownTOC -->

## Protocol

The service relies on a WebSocket connection to a specified endpoint that
exchanges JSON-encoded text messages. The communication is uni-directional - the
only messages are log messages that are sent from a connected client to the
server that aggregates the logs.

### Types

##### `LogLevel`

The log level encoded as a number. Possible values are:

- 0 - indicating `ERROR` level,
- 1 - indicating `WARN` level,
- 2 - indicating `INFO` level,
- 3 - indicating `DEBUG` level,
- 4 - indicating `TRACE` level.

```typescript
type LogLevel = 0 | 1 | 2 | 3 | 4;
```

##### `UTCTime`

Message timestamp encoded as milliseconds elapsed from the UNIX epoch, i.e.
1970-01-01T00:00:00Z.

```typescript
type UTCTime = number;
```

##### `Exception`

Encodes an exception that is related to a log message.

The `cause` field may be omitted if the exception does not have another
exception as its cause.

```typescript
interface Exception {
  // Name of the exception. In Java this can be the qualified classname.
  name: String;
  // Message associated with the exception. May be empty.
  message: String;
  // A stack trace indicating code location where the exception has originated
  // from. May be empty if unavailable.
  trace: [TraceElement];
  // Optional, another exception that caused this one.
  cause?: Exception;
}
```

##### `TraceElement`

Represents a single element of exception's stacktrace.

```typescript
interface TraceElement {
  // Name of the stack location. For example, in Java this can be a qualified
  // method name.
  element: String;
  // Code location of the element.
  location: String;
}
```

In Java, the location is usually a filename and line number locating the code
that corresponds to the indicated stack location, for example `Main.java:123`.
Native methods may be handled differently, as well as code from different
languages, for example Enso also includes the columns - `Test.enso:4:3-19`.

### Messages

Currently, the service supports only one message type - `LogMessage`, messages
not conforming to this format will be ignored. The first non-conforming message
for each connection will emit a warning.

#### `LogMessage`

Describes the log message that the server should report and does not expect any
response.

##### Parameters

```typescript
{
  // Log level associated with the message.
  level: LogLevel;
  // Timestamp indicating when the message was sent.
  time: UTCTime;
  // An identifier of a log group - the group should indicate which component
  // the message originated from and any (possibly nested) context.
  group: String;
  // The actual log message.
  message: String;
  // Optional exception associated with the message.
  exception?: Exception;
}
```

The `exception` field may be omitted if there is no exception associated with
the message.

In general, the `group` name can be arbitrary, but it is often the quallified
name of the class that the log message originates from and it is sometimes
extended with additional nested context, for example:

- `org.enso.launcher.cli.Main`
- `org.enso.compiler.pass.analyse.AliasAnalysis.analyseType`

### Examples

For example, an error message with an attached exception may look like this (the
class names are made up):

```json
{
  "level": 0,
  "time": 1600864353151,
  "group": "org.enso.launcher.Main",
  "message": "Failed to load a configuration file.",
  "exception": {
    "name": "org.enso.componentmanager.config.ConfigurationLoaderFailure",
    "message": "Configuration file does not exist.",
    "trace": [
      {
        "element": "org.enso.componentmanager.config.ConfigurationLoader.load",
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
`logging-service` provides syntactic sugar for working with nested logging
contexts.

```
package foo
import com.typesafe.scalalogging.Logger
import org.enso.logger.LoggerSyntax

class Foo {
  private val logger = Logger[Foo]

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

The `enter` extension method follows the convention that each level of context
nesting is separated by `.`, much like package names. The root context is
usually the qualified name of the relevant class, but other components are free
to use other conventions if needed.

### Setting Up Logging

The logger described above must know where it should send its logs, and this is
handled by the `LoggingServiceManager`. It allows to configure the logging
location, log level and setup the logging service in one of three different
modes:

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

In a rare situation where the service would not be initialized at all, a
shutdown hook is added that will print the pending log messages before exiting.
Some of the messages may be dropped, however, if more messages are buffered than
the buffer can hold.
