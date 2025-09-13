package org.enso.profiling.sampler;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.time.Instant;
import java.util.concurrent.atomic.AtomicInteger;
import org.netbeans.modules.sampler.Sampler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Gathers application performance statistics that can be visualised in Java VisualVM, and writes it
 * to the provided output.
 */
final class OutputStreamSampler implements MethodsSampler {

  private final Sampler sampler = Sampler.createSampler(this.getClass().getSimpleName());
  private final OutputStream npss;
  private final Writer events;
  private final AtomicInteger seq = new AtomicInteger();

  private boolean isSamplingStarted = false;

  private static final Logger LOGGER = LoggerFactory.getLogger(OutputStreamSampler.class);

  /**
   * Creates the {@link OutputStreamSampler} for provided output stream.
   *
   * @param npss the output stream to write result to.
   * @param events the log file with events
   * @throws IOException when it is not possible to write to the streams
   */
  OutputStreamSampler(OutputStream npss, OutputStream events) throws IOException {
    this.npss = npss;
    this.events = new OutputStreamWriter(events);
    this.events.write("<?xml version='1.0'?>\n");
    this.events.write("<records>\n");
  }

  @Override
  public void start() {
    synchronized (this) {
      if (sampler != null && !isSamplingStarted) {
        LOGGER.trace("Starting profiling sampler");
        sampler.start();
        isSamplingStarted = true;
      }
    }
  }

  @Override
  public void close() throws IOException {
    synchronized (this) {
      if (isSamplingStarted) {
        LOGGER.trace("Stopping profiling sampler");
        try (DataOutputStream dos = new DataOutputStream(npss)) {
          sampler.stopAndWriteTo(dos);
        }
        isSamplingStarted = false;
      }
    }
  }

  @Override
  public void log(Instant at, String message) {
    try {
      events.write("<record>\n");
      events.write("  <millis>" + at.toEpochMilli() + "</millis>\n");
      events.write("  <nanos>" + at.getNano() + "</nanos>\n");
      events.write("  <sequence>" + seq.incrementAndGet() + "</sequence>\n");
      events.write("  <level>INFO</level>\n");
      events.write("  <thread>1</thread>\n");
      events.write("  <message>" + xmlize(message) + "</message>\n");
      events.write("</record>\n");
    } catch (IOException ex) {
      LOGGER.warn("Cannot log event: " + message, ex);
    }
  }

  private static String xmlize(String message) {
    return message.replace("<", "&lt;").replace(">", "&gt;").replace("&", "&amp;");
  }
}
