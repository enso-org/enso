package org.enso.os.envitest;

import java.io.FileWriter;

/** Entry point for a "isolate library" to be loaded and communicated to via a {@link Channel}. */
public final class EnviMain {
  public static void main(String... args) throws Exception {
    try (java.io.FileWriter out = new FileWriter(args[0])) {
      out.write(args[1]);
    }
  }
}
