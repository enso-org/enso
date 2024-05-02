package org.enso.database.audit;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.enso.base.enso_cloud.audit.AuditLog;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.logging.Logger;

public final class CloudAuditedConnection extends AuditedConnection {
  private static final Logger logger = Logger.getLogger(CloudAuditedConnection.class.getName());
  private final ObjectNode metadata;

  public CloudAuditedConnection(Connection underlying, String relatedAssetId) {
    super(underlying);
    metadata = new ObjectNode(JsonNodeFactory.instance);
    if (relatedAssetId != null) {
      metadata.put("asset_id", relatedAssetId);
    }
    try {
      metadata.put("connection_uri", underlying.getMetaData().getURL());
    } catch (SQLException e) {
      // We ignore the exception, only logging it
      logger.warning("Failed to get connection URI for " + underlying + ": " + e.getMessage());
    }
  }

  private void audit(String operationType, String sql) {
    // TODO remove the print - for debugging
    System.out.println(operationType + " " + sql + " " + metadata);
    AuditLog.logAsync(operationType, sql, metadata);
  }

  @Override
  protected void auditQuery(String operationType, String sql) {
    audit(operationType, sql);
  }

  @Override
  protected void auditTransaction(String operation) {
    // As long as our logs don't guarantee ordering, reporting transaction commit/rollback is not too helpful.
    // So we just ignore it.
  }
}
