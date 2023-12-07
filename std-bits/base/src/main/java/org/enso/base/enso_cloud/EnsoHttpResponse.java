package org.enso.base.enso_cloud;

import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpHeaders;
import java.util.List;

/**
 * A subset of the HttpResponse to avoid leaking the decrypted Enso secrets.
 */
public record EnsoHttpResponse(
    URI uri,
    HttpHeaders headers,
    InputStream body,
    int statusCode) {
}
