package org.enso.aws.ses;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/** Helper for building MIME payloads for SES raw email sends. */
public final class EmailMimeBuilder {

  private EmailMimeBuilder() {}

  /**
   * Builds a minimal MIME message containing a single text part.
   *
   * @param from sender address (display names allowed)
   * @param to first recipient (used for the To header)
   * @param subject message subject
   * @param body text body content
   * @return serialized MIME message
   */
  public static byte[] buildSimpleMime(String from, String to, String subject, String body) {
    if (to == null || to.isEmpty()) {
      throw new IllegalArgumentException("A recipient is required.");
    }

    try (ByteArrayOutputStream out = new ByteArrayOutputStream();
         PrintWriter writer = new PrintWriter(out, false, StandardCharsets.UTF_8)) {
      
      // Write headers
      writer.println("From: " + from);
      writer.println("To: " + to);
      writer.println("Subject: " + subject);
      writer.println("Date: " + formatDate(new Date()));
      writer.println("MIME-Version: 1.0");
      writer.println("Content-Type: text/plain; charset=utf-8");
      writer.println("Content-Transfer-Encoding: 8bit");
      writer.println();
      
      // Write body
      writer.print(body);
      writer.flush();
      
      return out.toByteArray();
    } catch (IOException e) {
      throw new IllegalStateException("Unable to serialize MIME message.", e);
    }
  }

  private static String formatDate(Date date) {
    SimpleDateFormat dateFormat = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", Locale.US);
    dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    return dateFormat.format(date);
  }
}
