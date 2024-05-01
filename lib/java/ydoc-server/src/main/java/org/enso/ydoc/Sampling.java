package org.enso.ydoc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import org.enso.profiling.sampler.MethodsSampler;
import org.enso.profiling.sampler.OutputStreamSampler;

public class Sampling {

  private static final String SAMPLING_EXT = ".npss";

  private Sampling() {}

  public static void init() throws FileNotFoundException {
    var samplingPath = System.getProperty("sampling");
    if (samplingPath != null) {
      if (!samplingPath.endsWith(SAMPLING_EXT)) {
        samplingPath = samplingPath + SAMPLING_EXT;
      }
      var sampler = OutputStreamSampler.ofFile(new File(samplingPath));
      startSampling(sampler);
    }
  }

  private static void startSampling(MethodsSampler sampler) {
    sampler.start();
    Runtime.getRuntime()
        .addShutdownHook(
            new Thread(
                () -> {
                  try {
                    sampler.stop();
                  } catch (IOException e) {
                    throw new RuntimeException(e);
                  }
                }));
  }
}
