package org.enso.database.audit;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.logging.Logger;

public class LocalAuditedConnection extends AuditedConnection {
  private static final Logger logger = Logger.getLogger("Standard.Database.Connection");
  private final String connectionIdentifier;

  public LocalAuditedConnection(Connection underlying) {
    super(underlying);

    String connectionUri = null;
    try {
      connectionUri = underlying.getMetaData().getURL();
    } catch (SQLException e) {
      // We ignore the exception
    }

    this.connectionIdentifier = connectionUri == null ? underlying.toString() : connectionUri;
  }

  @Override
  protected void auditQuery(String operationType, String sql) {
    logger.info(connectionIdentifier + " - " + operationType + ": " + sql);
  }

  @Override
  protected void auditTransaction(String operation) {
    logger.info(connectionIdentifier + " - transaction - " + operation);
  }
}
