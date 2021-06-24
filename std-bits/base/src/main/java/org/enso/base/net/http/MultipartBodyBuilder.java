package org.enso.base.net.http;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.net.http.HttpRequest;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Supplier;

/** A builder for a multipart form data. */
public class MultipartBodyBuilder {
  private final List<PartsSpecification> partsSpecificationList = new ArrayList<>();
  private final String boundary = UUID.randomUUID().toString();

  /**
   * Create HTTP body publisher for a multipart form data.
   *
   * @return the body publisher.
   */
  public HttpRequest.BodyPublisher build() {
    if (partsSpecificationList.size() == 0) {
      throw new IllegalStateException("Must have at least one part to build multipart message.");
    }
    addFinalBoundaryPart();
    return HttpRequest.BodyPublishers.ofByteArrays(PartsIterator::new);
  }

  /**
   * Get the multipart boundary separator.
   *
   * @return the multipart boundary string.
   */
  public String get_boundary() {
    return boundary;
  }

  /**
   * Add text field to the multipart form.
   *
   * @param name the field name.
   * @param value the field value.
   * @return this builder.
   */
  public MultipartBodyBuilder add_part_text(String name, String value) {
    PartsSpecification newPart = new PartsSpecification();
    newPart.type = PartsSpecification.TYPE.STRING;
    newPart.name = name;
    newPart.value = value;
    partsSpecificationList.add(newPart);
    return this;
  }

  /**
   * Add file field to the multipart form.
   *
   * @param name the field name.
   * @param path the file path.
   * @return this builder.
   */
  public MultipartBodyBuilder add_part_file(String name, String path) {
    PartsSpecification newPart = new PartsSpecification();
    newPart.type = PartsSpecification.TYPE.FILE;
    newPart.name = name;
    newPart.path = Paths.get(path);
    partsSpecificationList.add(newPart);
    return this;
  }

  /**
   * Add the data field to the multipart form.
   *
   * @param name the field name.
   * @param value the field value.
   * @param filename the file name.
   * @param contentType the content type.
   * @return this builder.
   */
  public MultipartBodyBuilder add_part_bytes(
      String name, byte[] value, String filename, String contentType) {
    PartsSpecification newPart = new PartsSpecification();
    newPart.type = PartsSpecification.TYPE.STREAM;
    newPart.name = name;
    newPart.stream = () -> new ByteArrayInputStream(value);
    newPart.filename = filename;
    newPart.contentType = contentType;
    partsSpecificationList.add(newPart);
    return this;
  }

  private void addFinalBoundaryPart() {
    PartsSpecification newPart = new PartsSpecification();
    newPart.type = PartsSpecification.TYPE.FINAL_BOUNDARY;
    newPart.value = "--" + boundary + "--";
    partsSpecificationList.add(newPart);
  }

  private static final class PartsSpecification {

    public enum TYPE {
      STRING,
      FILE,
      STREAM,
      FINAL_BOUNDARY
    }

    TYPE type;
    String name;
    String value;
    Path path;
    Supplier<InputStream> stream;
    String filename;
    String contentType;
  }

  private final class PartsIterator implements Iterator<byte[]> {

    private final Iterator<PartsSpecification> iter;
    private InputStream currentFileInput;

    private boolean done;
    private byte[] next;

    PartsIterator() {
      iter = partsSpecificationList.iterator();
    }

    @Override
    public boolean hasNext() {
      if (done) return false;
      if (next != null) return true;
      try {
        next = computeNext();
      } catch (IOException e) {
        throw new UncheckedIOException(e);
      }
      if (next == null) {
        done = true;
        return false;
      }
      return true;
    }

    @Override
    public byte[] next() {
      if (!hasNext()) throw new NoSuchElementException();
      byte[] res = next;
      next = null;
      return res;
    }

    private byte[] computeNext() throws IOException {
      if (currentFileInput == null) {
        if (!iter.hasNext()) return null;
        PartsSpecification nextPart = iter.next();
        if (PartsSpecification.TYPE.STRING.equals(nextPart.type)) {
          String part =
              "--"
                  + boundary
                  + "\r\n"
                  + "Content-Disposition: form-data; name="
                  + nextPart.name
                  + "\r\n"
                  + "Content-Type: text/plain; charset=UTF-8\r\n\r\n"
                  + nextPart.value
                  + "\r\n";
          return part.getBytes(StandardCharsets.UTF_8);
        }
        if (PartsSpecification.TYPE.FINAL_BOUNDARY.equals(nextPart.type)) {
          return nextPart.value.getBytes(StandardCharsets.UTF_8);
        }
        String filename;
        String contentType;
        if (PartsSpecification.TYPE.FILE.equals(nextPart.type)) {
          Path path = nextPart.path;
          filename = path.getFileName().toString();
          contentType = Files.probeContentType(path);
          if (contentType == null) contentType = "application/octet-stream";
          currentFileInput = Files.newInputStream(path);
        } else {
          filename = nextPart.filename;
          contentType = nextPart.contentType;
          if (contentType == null) contentType = "application/octet-stream";
          currentFileInput = nextPart.stream.get();
        }
        String partHeader =
            "--"
                + boundary
                + "\r\n"
                + "Content-Disposition: form-data; name="
                + nextPart.name
                + "; filename="
                + filename
                + "\r\n"
                + "Content-Type: "
                + contentType
                + "\r\n\r\n";
        return partHeader.getBytes(StandardCharsets.UTF_8);
      } else {
        byte[] buf = new byte[8192];
        int length = currentFileInput.read(buf);
        if (length > 0) {
          byte[] actualBytes = new byte[length];
          System.arraycopy(buf, 0, actualBytes, 0, length);
          return actualBytes;
        } else {
          currentFileInput.close();
          currentFileInput = null;
          return "\r\n".getBytes(StandardCharsets.UTF_8);
        }
      }
    }
  }
}
