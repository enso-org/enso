package org.enso.base.net;

import org.apache.http.client.utils.URIBuilder;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

public class URIHelpers {
  public record NameValuePair(String name, String value) {}

  public static URI addQueryParameters(URI uri, List<NameValuePair> params) throws URISyntaxException {
    URIBuilder builder = new URIBuilder(uri);
    for (NameValuePair param : params) {
      builder.addParameter(param.name(), param.value());
    }
    return builder.build();
  }
}
