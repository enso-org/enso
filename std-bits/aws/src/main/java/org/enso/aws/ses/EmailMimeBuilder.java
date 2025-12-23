package org.enso.aws.ses;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Date;
import org.apache.james.mime4j.codec.DecodeMonitor;
import org.apache.james.mime4j.dom.TextBody;
import org.apache.james.mime4j.field.Fields;
import org.apache.james.mime4j.field.address.DefaultAddressParser;
import org.apache.james.mime4j.message.BasicBodyFactory;
import org.apache.james.mime4j.message.DefaultMessageWriter;
import org.apache.james.mime4j.message.MessageImpl;

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

    MessageImpl msg = new MessageImpl();
    var header = msg.getHeader();
    var addressParser = DefaultAddressParser.DEFAULT;

    try {
      header.setField(Fields.version("1.0"));
      header.setField(Fields.date(new Date()));
      header.setField(Fields.from(addressParser.parseMailbox(from, DecodeMonitor.SILENT)));
      header.setField(Fields.to(addressParser.parseMailbox(to, DecodeMonitor.SILENT)));
      header.setField(Fields.subject(subject));
      header.setField(Fields.contentType("text/plain; charset=UTF-8"));
      header.setField(Fields.contentTransferEncoding("8bit"));
    } catch (Exception ex) {
      throw new IllegalArgumentException("Invalid email header values.", ex);
    }

    BasicBodyFactory bf = new BasicBodyFactory(StandardCharsets.UTF_8);
    TextBody textBody = bf.textBody(body);
    msg.setBody(textBody);

    try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
      DefaultMessageWriter writer = new DefaultMessageWriter();
      writer.writeMessage(msg, out);
      return out.toByteArray();
    } catch (IOException e) {
      throw new IllegalStateException("Unable to serialize MIME message.", e);
    }
  }
}
