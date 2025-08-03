package org.enso.profiling.sampler;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
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
  private final OutputStream events;

  private boolean isSamplingStarted = false;

  private static final Logger LOGGER = LoggerFactory.getLogger(OutputStreamSampler.class);

  /**
   * Creates the {@link OutputStreamSampler} for provided output stream.
   *
   * @param npss the output stream to write result to.
   */
  OutputStreamSampler(OutputStream npss, OutputStream events) {
    this.npss = npss;
    this.events = events;
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
  public void log(String message) {}
}
