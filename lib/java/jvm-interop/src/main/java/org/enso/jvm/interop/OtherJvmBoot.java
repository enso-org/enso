package org.enso.jvm.interop;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Stream;
import org.enso.jvm.interop.impl.OtherJvmUtils;

final class OtherJvmBoot {
  private OtherJvmBoot() {}

  static void main(String... args) throws IOException, InterruptedException {
    if (args.length == 1 && "exit".equals(args[0])) {
      System.err.println("JVM started!");
      System.exit(0);
    }

    System.err.println("Trying to boot JVM with " + Arrays.toString(args) + " modules");
    var home = System.getProperty("java.home");
    if (home == null) {
      throw new IOException("No java.home specified");
    }
    var javaHome = new File(home);
    var java = new File(new File(javaHome, "bin"), "java");
    assert java.exists() : "Can execute " + java;
    var jvmArgs = OtherJvmUtils.findJvmArgs(javaHome, "org.enso.jvm.interop", Set.of(args));

    var allArgs =
        Stream.concat(
                Stream.of(java.getAbsolutePath()), // java executable
                Stream.concat(
                    Stream.of(jvmArgs), // JVM arguments
                    Stream.of("--list-modules") // print out all modules and exit
                    ))
            .toArray(String[]::new);

    System.err.println("Executing: " + Arrays.toString(allArgs));

    var pb = new ProcessBuilder(allArgs);
    pb.inheritIO();
    var p = pb.start();
    var res = p.waitFor();
    System.err.println("JVM exited with code " + res);
    System.exit(res);
  }
}
