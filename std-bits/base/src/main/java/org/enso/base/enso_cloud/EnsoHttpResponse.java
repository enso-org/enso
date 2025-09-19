package org.enso.base.enso_cloud;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpHeaders;
import java.util.zip.GZIPInputStream;

/**
 * A subset of the HttpResponse to avoid leaking the decrypted Enso secrets.
 *
 * <p>The {@code body} is transparently decoded based on the {@code Content-Encoding} header. It
 * supports only the "gzip" encoding. If other encodings are present, the body is left undecoded.
 */
public record EnsoHttpResponse(URI uri, HttpHeaders headers, InputStream body, int statusCode) {
  public EnsoHttpResponse {
    body = decodeContentEncoding(body, headers);
  }

  private static InputStream decodeContentEncoding(InputStream stream, HttpHeaders headers) {
    var encOpt = headers.firstValue("content-encoding");
    if (!encOpt.isEmpty() && "gzip".equals(encOpt.get().toLowerCase())) 
    {
        InputStream ret;
        try {
          ret = new GZIPInputStream(stream);
        } catch (IOException e) {
          // If we cannot decode, fall back to raw stream; consumers may handle errors.
          return stream;
        }
        return ret;
    }
    return stream;
  }
}
