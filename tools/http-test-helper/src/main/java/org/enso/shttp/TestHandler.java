package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.text.StringEscapeUtils;
import org.apache.http.NameValuePair;
import org.apache.http.client.utils.URIBuilder;

public class TestHandler extends SimpleHttpHandler {
  private final HttpMethod expectedMethod;
  private static final Set<String> ignoredHeaders = Set.of("Host");

  private static final Pattern textEncodingRegex = Pattern.compile(".*; charset=([^;]+).*");

  public TestHandler(HttpMethod expectedMethod) {
    this.expectedMethod = expectedMethod;
  }

  protected void doHandle(HttpExchange exchange) throws IOException {
    boolean first = true;
    String contentType = null;
    String textEncoding = "UTF-8";
    HttpMethod method;
    try {
      method = HttpMethod.valueOf(exchange.getRequestMethod());
    } catch (IllegalArgumentException e) {
      exchange.sendResponseHeaders(400, -1);
      return;
    }

    StringBuilder response;
    if (method == HttpMethod.HEAD || method == HttpMethod.OPTIONS) {
      response = new StringBuilder();
      exchange.sendResponseHeaders(200, -1);
    } else {
      if (method != expectedMethod) {
        exchange.sendResponseHeaders(405, -1);
        return;
      }

      exchange.getResponseHeaders().put("Content-Type", List.of("application/json"));
      response = new StringBuilder("{\n");
      response.append("  \"headers\": {\n");
      for (Map.Entry<String, List<String>> entry : exchange.getRequestHeaders().entrySet()) {
        if (!ignoredHeaders.contains(entry.getKey())) {
          if (!first) {
            response.append(",\n");
          } else {
            first = false;
          }
          response
              .append("    \"")
              .append(formatHeaderKey(entry.getKey()))
              .append("\": ")
              .append(formatHeaderValues(entry.getValue()));
        }
        if (entry.getKey().equals("Content-type")) {
          contentType = entry.getValue().get(0);
          String parsedTextEncoding = parseTextEncoding(contentType);
          if (parsedTextEncoding != null) {
            textEncoding = parsedTextEncoding;
          }
        }
      }

      URI uri = exchange.getRequestURI();

      response.append("\n");
      response.append("  },\n");
      response.append(
          "  \"origin\": \"" + exchange.getRemoteAddress().getAddress().getHostAddress() + "\",\n");
      response.append("  \"path\": \"" + StringEscapeUtils.escapeJson(uri.getPath()) + "\",\n");
      if (uri.getQuery() != null) {
        URIBuilder builder = new URIBuilder(uri);
        List<NameValuePair> params = builder.getQueryParams();
        response.append("  \"queryParameters\": [\n");
        for (int i = 0; i < params.size(); i++) {
          NameValuePair param = params.get(i);
          String key = StringEscapeUtils.escapeJson(param.getName());
          String value = StringEscapeUtils.escapeJson(param.getValue());
          response
              .append("    {\"name\": \"")
              .append(key)
              .append("\", \"value\": \"")
              .append(value)
              .append("\"}");
          boolean isLast = i == params.size() - 1;
          if (!isLast) {
            response.append(",\n");
          } else {
            response.append("\n");
          }
        }
        response.append("  ],\n");
      }

      response.append("  \"method\": \"").append(method).append("\",\n");
      if (method == HttpMethod.POST
          || method == HttpMethod.DELETE
          || method == HttpMethod.PUT
          || method == HttpMethod.PATCH) {
        response.append("  \"form\": null,\n");
        response.append("  \"files\": null,\n");
        String value = readBody(exchange.getRequestBody(), textEncoding);
        response
            .append("  \"data\": \"")
            .append(value == null ? "" : StringEscapeUtils.escapeJson(value))
            .append("\",\n");
      }
      response.append("  \"args\": {}\n");
      response.append("}");
      exchange.sendResponseHeaders(200, response.toString().getBytes().length);
      try (OutputStream os = exchange.getResponseBody()) {
        os.write(response.toString().getBytes());
      }
    }
  }

  private String readBody(InputStream inputStream, String encoding) {
    BufferedInputStream bis = new BufferedInputStream(inputStream);
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    try {
      while (true) {
        int c = bis.read();
        if (c == -1) {
          break;
        }
        baos.write((byte) c);
      }
    } catch (IOException ie) {
    }
    try {
      return baos.toString(encoding);
    } catch (UnsupportedEncodingException uee) {
      return "unsupported encoding";
    }
  }

  private String parseTextEncoding(String contentType) {
    Matcher matcher = textEncodingRegex.matcher(contentType);
    if (matcher.matches()) {
      return matcher.group(1);
    } else {
      return null;
    }
  }

  private String formatHeaderKey(String key) {
    int idx = key.indexOf('-');
    if (idx != -1 && key.length() >= idx) {
      return key.substring(0, idx + 1)
          + key.substring(idx + 1, idx + 2).toUpperCase()
          + key.substring(idx + 2);
    } else {
      return key;
    }
  }

  private String formatHeaderValues(List<String> key) {
    String merged = key.stream().reduce((a, b) -> a + ", " + b).orElse("");
    return "\"" + StringEscapeUtils.escapeJson(merged) + "\"";
  }
}
