package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.text.StringEscapeUtils;

public class TestHandler implements HttpHandler {
  private static final Set<String> ignoredHeaders = Set.of("Host");

  private static final Pattern textEncodingRegex = Pattern.compile(".*; charset=([^;]+).*");

  @Override
  public void handle(HttpExchange exchange) throws IOException {
    boolean first = true;
    String contentType = null;
    String textEncoding = "UTF-8";
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
          String parsedTextEncoding = parseTextEncoding(contentType);
          if (parsedTextEncoding != null) {
            textEncoding = parsedTextEncoding;
          }
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
        response += "  \"form\": null,\n";
        response += "  \"files\": null,\n";
        String value = readBody(exchange.getRequestBody(), textEncoding);
        response +=
            "  \"data\": \"" + (value == null ? "" : StringEscapeUtils.escapeJson(value)) + "\",\n";
      }
      response += "  \"args\": {}\n";
      response += "}";
      exchange.sendResponseHeaders(200, response.getBytes().length);
    }
    OutputStream os = exchange.getResponseBody();
    os.write(response.getBytes());
    os.close();
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
}
