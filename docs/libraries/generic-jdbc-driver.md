---
layout: developer-doc
title: Accessing Databases Through JDBC
category: libraries
tags: [libraries, databases]
order: 5
---

# Using JDBC with Enso

<!-- MarkdownTOC levels="2" autolink="true" -->

- [Overview](#overview)
- [Using JDBC Drivers](#using-jdbc-drivers)

<!-- /MarkdownTOC -->

## Overview

`Generic_JDBC_Connection` is a wrapper around a JDBC Connection. This can be
used to access any JDBC-compliant database backend, including ones that do not
have full support via `Connection`s. It provides schema information, and the
ability to execute SQL queries and commands.

Unlike regular fully-supported `Connection`s, query results are returned as
in-memory `Table`s, rather than as `DB_Table`s.

The JDBC driver must be available on the JVM classpath (for example, in a
`polyglot/java` folder within the project). JDBC drivers can only be used in JVM
mode.

## Using JDBC Drivers

### Set JVM Mode

To use JDBC, Enso has to run in 'JVM Mode' which allows it to load the drivers
at runtime.

Edit the `package.yaml` file in the root of your project directory and add a
line containing `jvm: true`:

```
name: MyProject
namespace: local
version: 0.0.1
prefer-local-libraries: 'true'
jvm: true
```

You must close and open the project in Enso for this change to take effect.

### Install The Driver

Download the JDBC driver and install it in the `polyglot/java` subdirectory of
the Enso project.

For example, after installing the JDBC driver for
[the H2 database](https://www.h2database.com/), your project struture would look
like this:

```
├── package.yaml
├── polyglot
│   └── java
│       └── h2-2.3.232.jar
└── src
    └── Main.enso
```

### Connect To The Database

In Enso, create a `Database.connect` node. Under `details`, select `JDBC`, and
enter the JDBC URL. The `Database.connect` node will look ike this:

![Database.connect details Generic_JDBC_Details.Value url 'jdbc:h2:/path/to/my/database'](images/connect_node.png)
