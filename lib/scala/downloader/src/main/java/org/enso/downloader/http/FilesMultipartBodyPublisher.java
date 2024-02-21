package org.enso.downloader.http;

import java.io.IOException;
import java.net.http.HttpRequest;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * This utility class adds support for {@code multipart/form-data} content-type as there is no such
 * support in {@code java.net.http} JDK.
 *
 * <p>Inspired by <a href="https://stackoverflow.com/a/56482187/4816269">SO</a>.
 */
public class FilesMultipartBodyPublisher {
  public static HttpRequest.BodyPublisher ofMimeMultipartData(
      Collection<Path> files, String boundary) throws IOException {
    // Result request body
    List<byte[]> byteArrays = new ArrayList<>();

    // Separator with boundary
    byte[] separator =
        ("--" + boundary + "\r\nContent-Disposition: form-data; name=\"files\";")
            .getBytes(StandardCharsets.UTF_8);

    for (Path path : files) {
      // Opening boundary
      byteArrays.add(separator);

      // If value is type of Path (file) append content type with file name and file binaries,
      // otherwise simply append key=value
      String mimeType = Files.probeContentType(path);
      byteArrays.add(
          (" filename=\"" + path.getFileName() + "\"\r\nContent-Type: " + mimeType + "\r\n\r\n")
              .getBytes(StandardCharsets.UTF_8));
      byteArrays.add(Files.readAllBytes(path));
      byteArrays.add("\r\n".getBytes(StandardCharsets.UTF_8));
    }

    // Closing boundary
    byteArrays.add(("--" + boundary + "--").getBytes(StandardCharsets.UTF_8));

    return HttpRequest.BodyPublishers.ofByteArrays(byteArrays);
  }
}
