package org.enso.downloader.http;

import java.net.URI;
import java.net.URISyntaxException;

/**
 * @see java.net.JarURLConnection
 */
public final class JarURIBuilder implements URIBuilder {
  private final URI uri;

  JarURIBuilder(URI uri) {
    assert uri.getScheme().equals("jar");
    this.uri = uri;
  }

  @Override
  public URIBuilder addPathSegment(String segment) {
    var ssp = uri.getSchemeSpecificPart();
    String newSsp;
    if (ssp.endsWith("/")) {
      newSsp = ssp + segment;
    } else {
      newSsp = ssp + "/" + segment;
    }
    URI newUri;
    try {
      newUri = new URI(uri.getScheme(), newSsp, uri.getFragment());
    } catch (URISyntaxException e) {
      throw new IllegalStateException(e);
    }
    return new JarURIBuilder(newUri);
  }

  @Override
  public URIBuilder addQuery(String key, String value) {
    throw new IllegalStateException("Should not be called on JAR URIs");
  }

  @Override
  public URI build() {
    return uri;
  }
}
