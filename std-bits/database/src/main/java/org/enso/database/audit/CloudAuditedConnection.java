package org.enso.database.audit;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.logging.Logger;
import org.enso.base.enso_cloud.audit.AuditLog;

public final class CloudAuditedConnection extends AuditedConnection {
  private static final Logger logger = Logger.getLogger(CloudAuditedConnection.class.getName());
  private final ObjectNode metadata;

  /**
   * Because logs are sent asynchronously and their timestamps are only assigned on the receiving
   * end, we need to assign a sequence number to be able to know the ordering between various
   * events.
   */
  private long sequenceNumber = 1;

  public CloudAuditedConnection(Connection underlying, String relatedAssetId) {
    super(underlying);
    metadata = new ObjectNode(JsonNodeFactory.instance);
    if (relatedAssetId != null) {
      metadata.put("dataLinkAssetId", relatedAssetId);
    }
    try {
      metadata.put("connectionUri", underlying.getMetaData().getURL());
    } catch (SQLException e) {
      // We ignore the exception, only logging it
      logger.warning("Failed to get connection URI for " + underlying + ": " + e.getMessage());
    }
  }

  private void audit(String operationType, String message) {
    var metadataCopy = metadata.deepCopy();
    metadataCopy.put("sequenceNumber", sequenceNumber++);
    AuditLog.logAsync(operationType, message, metadataCopy);
  }

  @Override
  protected void auditQuery(String operationType, String sql) {
    audit(operationType, sql);
  }

  @Override
  protected void auditTransaction(String operation) {
    audit("transaction", operation);
  }
}
