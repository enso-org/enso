package org.enso.shttp.cloud_mock;

import java.io.IOException;

/** We mostly don't test directories in the mock, but it is needed for resolving secrets. */
public class DirectoriesHandler implements CloudHandler {

  private static final String DIRECTORIES = "directories/";
  static final String DEFAULT_ROOT_ID = "directory-27xJM00p8jWoL2qByTo6tQfciWC";
  @Override
  public boolean canHandle(String subPath) {
    return subPath.startsWith(DIRECTORIES);
  }

  @Override
  public void handleCloudAPI(CloudExchange exchange) throws IOException {
    String id = exchange.subPath().substring(DIRECTORIES.length());
    System.out.println("id = " + id);
    if (id.equals(DEFAULT_ROOT_ID)) {
      exchange.sendResponse(200, defaultRootDirectory);
    } else {
      exchange.sendResponse(404, "No handler found for: " + id);
    }
  }

  private final String defaultRootDirectory = """
      {
        "title": "",
        "id": "%s",
      }
      """.formatted(DEFAULT_ROOT_ID);
}
