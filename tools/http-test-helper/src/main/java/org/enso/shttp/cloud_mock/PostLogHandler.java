package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import org.enso.shttp.HttpMethod;

public class PostLogHandler implements CloudHandler {
  private final UsersService users;
  private final EventsService events;
  private final ObjectMapper jsonMapper = new ObjectMapper();

  public PostLogHandler(UsersService users, EventsService events) {
    this.users = users;
    this.events = events;
  }

  @Override
  public boolean canHandle(String subPath) {
    return subPath.equals("logs");
  }

  @Override
  public void handleCloudAPI(CloudExchange exchange) throws IOException {
    if (exchange.getMethod() != HttpMethod.POST) {
      exchange.sendResponse(405, "Method Not Allowed");
      return;
    }

    JsonNode root = jsonMapper.readTree(exchange.decodeBodyAsText());
    String message = root.get("message").asText();
    String organizationId = users.currentUserOrganizationId();
    String userEmail = users.currentUserEmail();
    String timestamp = ZonedDateTime.now().withZoneSameInstant(ZoneId.of("UTC")).toString();
    JsonNode metadata = root.get("metadata");
    String projectId = root.get("projectId").asText();
    EventsService.LogEvent event =
        new EventsService.LogEvent(
            organizationId, userEmail, timestamp, metadata, message, projectId);
    events.recordEvent(event);
    exchange.sendResponse(204, "");
  }
}
