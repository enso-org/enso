package org.enso.interpreter.util;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleFile.FileTypeDetector;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import org.apache.tika.detect.DefaultEncodingDetector;
import org.apache.tika.detect.EncodingDetector;
import org.enso.polyglot.LanguageInfo;

/** A detector for finding a {@link TruffleFile file's} MIME type and encoding. */
public final class FileDetector implements TruffleFile.FileTypeDetector {

  /**
   * Finds the MIME type for a given {@link TruffleFile}.
   *
   * @param file the {@link TruffleFile file} to find a MIME type for
   * @return the MIME type or {@code null} if the MIME type is not recognized
   */
  @Override
  public String findMimeType(TruffleFile file) {
    String name = file.getName();
    if (name != null && name.endsWith(LanguageInfo.FILE_EXTENSION)) {
      return LanguageInfo.MIME_TYPE;
    }
    return null;
  }

  /**
   * For a file containing an encoding information returns the encoding.
   *
   * <p>Enso uses Apache Tika for determining the encoding for a provided file, but this is an
   * inexact science at best.
   *
   * @param file the {@link TruffleFile file} to find an encoding for. It's guaranteed that the
   *     {@code file} has a MIME type supported by the language registering this {@link
   *     FileTypeDetector}.
   * @return the file encoding or {@code null} if the file does not provide encoding
   * @throws IOException if an I/O error occurs
   */
  @Override
  public Charset findEncoding(TruffleFile file) throws IOException {
    InputStream fileReader = file.newInputStream();
    EncodingDetector detector = new DefaultEncodingDetector();
    return detector.detect(fileReader, null);
  }
}
