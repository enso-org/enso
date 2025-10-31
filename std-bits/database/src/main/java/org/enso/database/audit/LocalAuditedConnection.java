package org.enso.database.audit;

import java.sql.Connection;
import java.sql.SQLException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LocalAuditedConnection extends AuditedConnection {
  private static final Logger LOGGER = LoggerFactory.getLogger("Standard.Database.Connection");
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
    LOGGER.info(connectionIdentifier + " - {}: {}", operationType, sql);
  }

  private static final String operationType = "transaction";

  @Override
  protected void auditTransaction(String operation) {
    auditQuery(operationType, operation);
  }
}
