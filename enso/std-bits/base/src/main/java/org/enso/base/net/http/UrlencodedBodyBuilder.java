package org.enso.base.net.http;

import java.io.IOException;
import java.net.URLEncoder;
import java.net.http.HttpRequest;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;

/** A builder for an url-encoded form data. */
public final class UrlencodedBodyBuilder {

  private final ArrayList<String> parts = new ArrayList<>();

  /**
   * Create HTTP body publisher for an url-encoded form data.
   *
   * @return the body publisher.
   */
  public HttpRequest.BodyPublisher build() {
    String contents = String.join("&", parts);
    return HttpRequest.BodyPublishers.ofString(contents);
  }

  /**
   * Add text field to the form.
   *
   * @param name the field name.
   * @param value the field value.
   * @return this builder.
   */
  public UrlencodedBodyBuilder add_part_text(String name, String value) {
    parts.add(encodePart(name, value));
    return this;
  }

  /**
   * Add file field to the form.
   *
   * @param name the field name.
   * @param path the file path.
   * @return this builder.
   */
  public UrlencodedBodyBuilder add_part_file(String name, String path) throws IOException {
    String contents = Files.readString(Paths.get(path), StandardCharsets.UTF_8);
    parts.add(encodePart(name, contents));
    return this;
  }

  private String encodePart(String name, String value) {
    return
        URLEncoder.encode(name, StandardCharsets.UTF_8)
            + "="
            + URLEncoder.encode(value, StandardCharsets.UTF_8);
  }

}
