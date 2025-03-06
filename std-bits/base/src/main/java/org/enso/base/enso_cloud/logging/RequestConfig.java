package org.enso.base.enso_cloud.logging;

import java.net.URI;
import org.enso.base.enso_cloud.AuthenticationProvider;

/**
 * Contains information needed to build a request to the Cloud Logs API.
 *
 * <p>This information must be gathered on the main Enso thread, as only there we have access to the
 * {@link AuthenticationProvider}.
 *
 * <p>We associate an instance with every message to be sent. When sending multiple messages in a
 * batch, we will use the config from one of them. This should not matter as in normal operations
 * the configs will be the same, they only change during testing. Tests should this into account, by
 * sending the last message in synchronous mode.
 */
record RequestConfig(URI apiUri, String accessToken) {}
