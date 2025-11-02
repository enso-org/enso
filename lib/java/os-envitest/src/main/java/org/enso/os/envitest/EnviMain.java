package org.enso.os.envitest;

import java.util.Arrays;

/** Entry point for a "isolate library" to be loaded and communicated to via a 
 * {@link Channel}.
 */
public final class EnviMain {
  public static void main(String... args) {
    System.err.println("ENVI!!!! Main: " + Arrays.toString(args));
  }
}
