package org.enso.downloader.http;

import java.io.IOException;
import java.math.BigInteger;
import java.net.http.HttpRequest;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

/**
 * This utility class adds support for {@code multipart/form-data} content-type as there is no such
 * support in {@code java.net.http} JDK.
 *
 * <p>Copied from <a href="https://stackoverflow.com/a/56482187/4816269">SO</a>.
 */
public class MultipartBodyPublisher {
  private static final String BOUNDARY = new BigInteger(256, new Random()).toString();

  public static HttpRequest.BodyPublisher ofMimeMultipartData(Map<Object, Object> data)
      throws IOException {
    // Result request body
    List<byte[]> byteArrays = new ArrayList<>();

    // Separator with boundary
    byte[] separator =
        ("--" + BOUNDARY + "\r\nContent-Disposition: form-data; name=")
            .getBytes(StandardCharsets.UTF_8);

    for (Map.Entry<Object, Object> entry : data.entrySet()) {

      // Opening boundary
      byteArrays.add(separator);

      // If value is type of Path (file) append content type with file name and file binaries,
      // otherwise simply append key=value
      if (entry.getValue() instanceof Path path) {
        String mimeType = Files.probeContentType(path);
        byteArrays.add(
            ("\""
                    + entry.getKey()
                    + "\"; filename=\""
                    + path.getFileName()
                    + "\"\r\nContent-Type: "
                    + mimeType
                    + "\r\n\r\n")
                .getBytes(StandardCharsets.UTF_8));
        byteArrays.add(Files.readAllBytes(path));
        byteArrays.add("\r\n".getBytes(StandardCharsets.UTF_8));
      } else {
        byteArrays.add(
            ("\"" + entry.getKey() + "\"\r\n\r\n" + entry.getValue() + "\r\n")
                .getBytes(StandardCharsets.UTF_8));
      }
    }

    // Closing boundary
    byteArrays.add(("--" + BOUNDARY + "--").getBytes(StandardCharsets.UTF_8));

    // Serializing as byte array
    return HttpRequest.BodyPublishers.ofByteArrays(byteArrays);
  }
}
