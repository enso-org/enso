package org.enso.os.environment.jni;

import java.io.File;
import java.io.FileWriter;

public final class TestMain {
  private TestMain() {}

  public static void main(String... args) throws Exception {
    var out = new File(args[0]);
    try (java.io.FileWriter os = new FileWriter(out)) {
      os.write("Ciao");
    }
  }
}
