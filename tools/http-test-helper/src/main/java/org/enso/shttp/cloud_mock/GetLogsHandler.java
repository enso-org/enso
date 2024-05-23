package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.List;

public class GetLogsHandler implements CloudHandler {
  private final EventsService events;
  private final ObjectMapper jsonMapper = new ObjectMapper();

  public GetLogsHandler(EventsService events) {
    this.events = events;
  }

  @Override
  public boolean canHandle(String subPath) {
    return subPath.equals("log_events");
  }

  @Override
  public void handleCloudAPI(CloudExchange exchange) throws IOException {
    var response = new GetLogEventsResponse(events.getEvents());
    exchange.sendResponse(200, jsonMapper.writeValueAsString(response));
  }

  private record GetLogEventsResponse(List<EventsService.LogEvent> events) {}
}
