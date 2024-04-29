package org.enso.database.audit;

import java.sql.Connection;

public final class CloudAuditedConnection extends AuditedConnection {
  CloudAuditedConnection(Connection underlying) {
    super(underlying);
  }

  @Override
  protected void audit(String sql) {
    // TODO
  }
}
