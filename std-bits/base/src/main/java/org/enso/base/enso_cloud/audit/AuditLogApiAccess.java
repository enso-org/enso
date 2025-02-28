package org.enso.base.enso_cloud.audit;

import java.net.URI;
import org.enso.base.enso_cloud.CloudAPI;
import org.enso.base.enso_cloud.logging.LogApiAccess;

/**
 * Gives access to the low-level log event API in the Cloud and manages asynchronously submitting
 * the logs.
 */
final class AuditLogApiAccess extends LogApiAccess {

  @Override
  public URI endpoint() {
    return URI.create(CloudAPI.getAPIRootURI() + "logs");
  }
}
