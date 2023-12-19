package org.enso.shttp.cloud_mock;

import java.io.IOException;

public class UsersHandler implements CloudHandler {

  private static final String USERS = "users";

  @Override
  public boolean canHandle(String subPath) {
    return subPath.startsWith(USERS);
  }

  @Override
  public void handleCloudAPI(CloudExchange exchange) throws IOException {
    String part = exchange.subPath().substring(USERS.length());
    switch (part) {
      case "/me" -> sendCurrentUser(exchange);
      case "" -> sendUserList(exchange);
      default -> {
        exchange.sendResponse(404, "No handler found for: " + part);
      }
    }
  }

  private void sendCurrentUser(CloudExchange exchange) throws IOException {
    exchange.sendResponse(200, currentUser);
  }

  private void sendUserList(CloudExchange exchange) throws IOException {
    String response =
        """
        {
            "users": [
                %s,
                %s
            ]
        }
        """
            .formatted(currentUser, otherUser);
    exchange.sendResponse(200, response);
  }

  private final String currentUser =
      """
      {
          "id": "organization-27xJM00p8jWoL2qByTo6tQfciWC",
          "name": "My test User 1",
          "email": "enso-test-user-1@example.com",
          "isEnabled": true,
          "rootDirectoryId": "directory-27xJM00p8jWoL2qByTo6tQfciWC"
      }
      """;

  private final String otherUser =
      """
      {
          "id": "organization-44AAA00A8AAAA2AAAAA6AAAAAAA",
          "name": "My test User 2",
          "email": "enso-test-user-2@example.com",
          "isEnabled": false,
          "rootDirectoryId": "directory-44AAA00A8AAAA2AAAAA6AAAAAAA"
      }
      """;
}
