package org.enso.base.enso_cloud;

import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpHeaders;

/**
 * A subset of the HttpResponse to avoid leaking the decrypted Enso secrets.
 *
 * <p>The {@code body} is transparently decoded based on the {@code Content-Encoding} header. It
 * supports only the "gzip" encoding. If other encodings are present, the body is left undecoded.
 */
public record EnsoHttpResponse(URI uri, HttpHeaders headers, InputStream body, int statusCode) {}
