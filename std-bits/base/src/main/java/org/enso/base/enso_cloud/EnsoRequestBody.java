package org.enso.base.enso_cloud;

import java.io.FileNotFoundException;
import java.net.http.HttpRequest;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

/** An interface for Standard.Base.Network.HTTP.Request_Body.Request_Body can match */
public interface EnsoRequestBody {
  String type_name();

  String text_value();

  String charset();

  byte[] bytes_value();

  static HttpRequest.BodyPublisher build(EnsoRequestBody body)
      throws FileNotFoundException, UnsupportedOperationException {
    return switch (body.type_name()) {
      case "Empty" -> HttpRequest.BodyPublishers.noBody();
      case "Text" -> {
        var charsetToUse = body.charset();
        yield charsetToUse == null
            ? HttpRequest.BodyPublishers.ofString(body.text_value())
            : HttpRequest.BodyPublishers.ofString(body.text_value(), Charset.forName(charsetToUse));
      }
      case "Json" -> HttpRequest.BodyPublishers.ofString(body.text_value());
      case "Binary" -> HttpRequest.BodyPublishers.ofFile(Path.of(body.text_value()));
      case "ByteArray" -> HttpRequest.BodyPublishers.ofByteArray(body.bytes_value());
      default ->
          throw new UnsupportedOperationException(
              "Cannot build request body for " + body.type_name());
    };
  }

  static byte[] hashInput(EnsoRequestBody body) throws UnsupportedOperationException {
    return switch (body.type_name()) {
      case "Empty" -> new byte[0];
      case "Text" -> {
        var charsetToUse = body.charset();
        yield charsetToUse == null
            ? body.text_value().getBytes(StandardCharsets.UTF_8)
            : body.text_value().getBytes(Charset.forName(charsetToUse));
      }
      case "Json" -> body.text_value().getBytes(StandardCharsets.UTF_8);
      case "ByteArray" -> body.bytes_value();
      default ->
          throw new UnsupportedOperationException(
              "Hashing a " + body.type_name() + " body is not yet supported.");
    };
  }
}
