package org.enso.shttp.test_helpers;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import org.apache.http.client.utils.URIBuilder;
import org.enso.shttp.SimpleHttpHandler;

public class CrashingTestHandler extends SimpleHttpHandler {
  private int requests = 0;

  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    // Exceptions will be logged by SimpleHttpHandler, but that's OK - let's know that this
    // crash is happening.

    URI uri = exchange.getRequestURI();
    URIBuilder builder = new URIBuilder(uri);
    final String successEveryParam = "success_every";
    final String crashTypeParam = "type";

    int successEvery = 0;
    CrashType crashType = CrashType.RUNTIME;

    for (var queryPair : builder.getQueryParams()) {
      if (queryPair.getName().equals(successEveryParam)) {
        successEvery = Integer.decode(queryPair.getValue());
      } else if (queryPair.getName().equals(crashTypeParam)) {
        crashType =
            switch (queryPair.getValue()) {
              case "stream" -> CrashType.STREAM;
              default -> CrashType.RUNTIME;
            };
      }
    }
    if (successEvery == 0) {
      // Reset counter
      requests = 0;
    }

    boolean shouldSucceed = successEvery == (requests + 1);

    switch (crashType) {
      case RUNTIME:
        if (shouldSucceed) {
          // return OK, reset
          requests = 0;
          exchange.sendResponseHeaders(200, -1);
          exchange.close();
          break;
        } else {
          requests += 1;
          throw new RuntimeException("This handler crashes on purpose.");
        }

      case STREAM:
        byte[] responseData = "Crash and Burn".getBytes();
        exchange.sendResponseHeaders(200, responseData.length);
        try {
          if (shouldSucceed) {
            requests = 0;
            // return OK, reset
            try (OutputStream os = exchange.getResponseBody()) {
              os.write(responseData, 0, responseData.length);
            }
          } else {
            requests += 1;
            try (OutputStream os = exchange.getResponseBody()) {
              os.write(responseData, 0, responseData.length / 2);
            }
          }
        } finally {
          exchange.close();
        }
        break;
    }
  }

  enum CrashType {
    RUNTIME, // Simulate an internal server crash
    STREAM // Simulate a crash by abruptly closing response's body stream
  }
}
