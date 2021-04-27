package org.enso.searcher.sql

class InvalidSchemaVersion(val version: Long)
    extends RuntimeException(
      s"Database schema version '$version' is different from the application " +
      s"schema version '${SchemaVersion.CurrentVersion}'."
    )
