package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.enso.shttp.HttpMethod;

import java.io.IOException;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;

public class PostLogHandler implements CloudHandler {
  private final UsersService usersService;
  private final EventsService eventsService;
  private final ObjectMapper jsonMapper = new ObjectMapper();

  public PostLogHandler(UsersService usersService, EventsService eventsService) {
    this.usersService = usersService;
    this.eventsService = eventsService;
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

    try {
      // Delay recording the event to simulate network conditions
      Thread.sleep(100);
    } catch (InterruptedException e) {
      // ignore the interruption
    }

    JsonNode root = jsonMapper.readTree(exchange.decodeBodyAsText());
    var incomingEvents = decodeLogEvents(root);
    if (incomingEvents.isEmpty()) {
      exchange.sendResponse(400, "Empty array was sent.");
      return;
    }
    for (var event : incomingEvents) {
      eventsService.recordEvent(event);
    }
    exchange.sendEmptyResponse(204);
  }

  private List<EventsService.LogEvent> decodeLogEvents(JsonNode root) {
    if (root.isArray()) {
      List<EventsService.LogEvent> events = new ArrayList<>(root.size());
      for (JsonNode event : root) {
        events.add(parseLogEvent(event));
      }
      return events;
    } else if (root.isObject()) {
      return List.of(parseLogEvent(root));
    } else {
      throw new IllegalArgumentException("Invalid JSON structure: " + root);
    }
  }

  private EventsService.LogEvent parseLogEvent(JsonNode json) {
    String message = json.get("message").asText();
    String organizationId = usersService.currentUserOrganizationId();
    String userEmail = usersService.currentUserEmail();
    String timestamp = ZonedDateTime.now().withZoneSameInstant(ZoneId.of("UTC")).toString();
    JsonNode metadata = json.get("metadata");
    String projectId = json.get("projectId").asText();
    return new EventsService.LogEvent(
            organizationId, userEmail, timestamp, metadata, message, projectId);
  }
}
