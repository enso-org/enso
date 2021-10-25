package org.enso.searcher.sql

/** An error indicating that the database has invalid schema version.
  *
  * @param version the database schema version.
  */
class InvalidSchemaVersion(val version: Long)
    extends RuntimeException(
      s"Database schema version '$version' is different from the application " +
      s"schema version '${SchemaVersion.CurrentVersion}'."
    )
