package org.enso.shttp.cloud_mock;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;

public interface CloudHandler {
  boolean canHandle(String subPath);

  void handleCloudAPI(CloudExchange exchange) throws IOException;

  interface CloudExchange {
    HttpExchange getHttpExchange();

    String subPath();

    void sendResponse(int code, String response) throws IOException;
  }
}
