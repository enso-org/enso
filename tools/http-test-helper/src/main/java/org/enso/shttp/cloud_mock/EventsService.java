package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;

public class EventsService {
  public record LogEvent(
      String organizationId,
      String userEmail,
      String timestamp,
      JsonNode metadata,
      String message,
      String projectId) {}

  public void recordEvent(LogEvent event) {
    events.add(event);
  }

  public List<LogEvent> getEvents() {
    return new ArrayList<>(events);
  }

  private final ArrayList<LogEvent> events = new ArrayList<>();
}
