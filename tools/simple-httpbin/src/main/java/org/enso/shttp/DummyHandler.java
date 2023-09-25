package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.text.StringEscapeUtils;

public class DummyHandler implements HttpHandler {
  private static final Set<String> ignoredHeaders = Set.of("Host");

  @Override
  public void handle(HttpExchange exchange) throws IOException {
    boolean first = true;
    String contentType = null;
    HttpMethod meth = HttpMethod.valueOf(exchange.getRequestMethod());

    String response;
    if (meth == HttpMethod.HEAD || meth == HttpMethod.OPTIONS) {
      response = "";
      exchange.sendResponseHeaders(200, -1);
    } else {
      exchange.getResponseHeaders().put("Content-Type", List.of("application/json"));
      response = "{\n";
      response += "  \"headers\": {\n";
      for (Map.Entry<String, List<String>> entry : exchange.getRequestHeaders().entrySet()) {
        if (!ignoredHeaders.contains(entry.getKey())) {
          if (!first) {
            response += ",\n";
          } else {
            first = false;
          }
          response +=
              "    \""
                  + formatHeaderKey(entry.getKey())
                  + "\": \""
                  + entry.getValue().get(0)
                  + "\"";
        }
        if (entry.getKey().equals("Content-type")) {
          contentType = entry.getValue().get(0);
        }
      }
      response += "\n";
      response += "  },\n";
      response += "  \"origin\": \"127.0.0.1\",\n";
      response += "  \"url\": \"\",\n";
      response += "  \"method\": \"" + meth + "\",\n";
      if (meth == HttpMethod.POST
          || meth == HttpMethod.DELETE
          || meth == HttpMethod.PUT
          || meth == HttpMethod.PATCH) {
        boolean isJson = contentType != null && contentType.equals("application/json");
        InputStreamReader isr = new InputStreamReader(exchange.getRequestBody(), "utf-8");
        BufferedReader br = new BufferedReader(isr);
        String value = br.readLine();
        response += "  \"form\": null,\n";
        response += "  \"files\": null,\n";
        response +=
            "  \"data\": \"" + (value == null ? "" : StringEscapeUtils.escapeJson(value)) + "\",\n";
        response += "  \"json\": " + (isJson ? value : "null") + ",\n";
      }
      response += "  \"args\": {}\n";
      response += "}";
      exchange.sendResponseHeaders(200, response.getBytes().length);
    }
    OutputStream os = exchange.getResponseBody();
    os.write(response.getBytes());
    os.close();
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
}
