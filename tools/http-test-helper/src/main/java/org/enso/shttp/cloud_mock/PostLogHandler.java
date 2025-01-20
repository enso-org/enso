package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import org.enso.shttp.HttpMethod;

public class PostLogHandler implements CloudHandler {
  private final UsersService usersService;
  private final EventsService eventsService;
  private final ObjectMapper jsonMapper = new ObjectMapper();

  /**
   * Parameter indicating if manual batching test mode is enabled.
   *
   * <p>In this mode, the server will sleep during processing of log messages, delaying the
   * processing, giving the Enso test process time to gather more messages that need to be sent. The
   * process will also log how many log messages were received in each batch. Enabling this flag and
   * inspecting these logs allow to manually test that messages are indeed sent in batches if the
   * log processing is slower.
   */
  private final boolean batchingTestModeEnabled;

  public PostLogHandler(
      UsersService usersService, EventsService eventsService, boolean batchingTestModeEnabled) {
    this.usersService = usersService;
    this.eventsService = eventsService;
    this.batchingTestModeEnabled = batchingTestModeEnabled;
    if (batchingTestModeEnabled) {
      System.out.println("Manual audit log batching test mode enabled.");
    }
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
    var incomingEvents = decodeLogEvents(root);
    if (batchingTestModeEnabled) {
      System.out.println("Received a batch of " + incomingEvents.size() + " audit log messages.");
      try {
        Thread.sleep(1000);
      } catch (InterruptedException e) {
        // ignore the interruption
      }
    }
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
    if (!root.isObject()) {
      throw new IllegalArgumentException("Invalid JSON structure: " + root);
    }

    if (root.has("logs") && root.size() == 1) {
      var array = root.get("logs");
      if (!array.isArray()) {
        throw new IllegalArgumentException("Invalid JSON structure: " + root);
      }

      List<EventsService.LogEvent> events = new ArrayList<>(array.size());
      for (JsonNode event : array) {
        events.add(parseLogEvent(event));
      }
      return events;
    } else {
      return List.of(parseLogEvent(root));
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
