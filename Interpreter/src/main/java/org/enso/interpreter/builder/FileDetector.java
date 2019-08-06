package org.enso.interpreter.builder;

import com.oracle.truffle.api.TruffleFile;
import org.enso.interpreter.Constants;

import java.io.IOException;
import java.nio.charset.Charset;

public final class FileDetector implements TruffleFile.FileTypeDetector {

  @Override
  public String findMimeType(TruffleFile file) throws IOException {
    String name = file.getName();
    if (name != null && name.endsWith(Constants.FILE_EXTENSION)) {
      return Constants.MIME_TYPE;
    }
    return null;
  }

  @Override
  public Charset findEncoding(TruffleFile file) throws IOException {
    // TODO [AA] Give this a proper implementation.
    return null;
  }
}
