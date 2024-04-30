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

  CloudAuditedConnection(Connection underlying, String relatedAssetId) {
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

  @Override
  protected void auditQuery(String operationType, String sql) {
    AuditLog.logAsync(operationType, sql, metadata);
  }

  @Override
  protected void auditTransaction(String operation) {
    AuditLog.logAsync("transaction", operation, metadata);
  }
}
