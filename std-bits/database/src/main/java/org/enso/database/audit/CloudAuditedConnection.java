package org.enso.database.audit;

import org.enso.base.enso_cloud.audit.AuditLogAPI;

import java.sql.Connection;

public final class CloudAuditedConnection extends AuditedConnection {
  CloudAuditedConnection(Connection underlying) {
    super(underlying);
  }

  @Override
  protected void audit(String sql) {
    var message = new DatabaseLogMessage("TODO", "TODO", sql);
    AuditLogAPI.INSTANCE.logAsync(message);
  }

  private static class DatabaseLogMessage implements AuditLogAPI.LogMessage {

    private final String databaseHost;
    private final String databaseName;
    private final String sql;

    private DatabaseLogMessage(String databaseHost, String databaseName, String sql) {
      this.databaseHost = databaseHost;
      this.databaseName = databaseName;
      this.sql = sql;
    }


    @Override
    public String payload() {
      return "TODO";
    }
  }
}
