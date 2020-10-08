package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.TruffleFile;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.OpenOption;

public class EnsoFile {
  private final TruffleFile truffleFile;

  public EnsoFile(TruffleFile truffleFile) {
    this.truffleFile = truffleFile;
  }

  public BufferedReader newBufferedReader() throws IOException {
    return truffleFile.newBufferedReader();
  }

  public OutputStream newOutputStream(OpenOption[] opts) throws IOException {
    return truffleFile.newOutputStream(opts);
  }

  public InputStream newInputStream(OpenOption[] opts) throws IOException {
    return truffleFile.newInputStream(opts);
  }

  public EnsoFile resolve(String subPath) {
    return new EnsoFile(this.truffleFile.resolve(subPath));
  }

  public EnsoFile resolve(EnsoFile subPath) {
    return new EnsoFile(this.truffleFile.resolve(subPath.truffleFile.getPath()));
  }

  @Override
  public String toString() {
    return "(File " + truffleFile.getPath() + ")";
  }
}
