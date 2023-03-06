package org.enso.base_test_helpers;

import java.io.File;

public class FileSystemHelper {
  public static void setWritable(String path, boolean writable) {
    File file = new File(path);
    if (!file.setWritable(writable)) {
      throw new UnsupportedOperationException(
          "Failed to set file " + file + " writable: " + writable);
    }
  }
}
