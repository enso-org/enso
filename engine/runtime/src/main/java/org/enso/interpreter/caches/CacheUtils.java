package org.enso.interpreter.caches;

import com.oracle.truffle.api.TruffleFile;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Comparator;
import java.util.List;
import java.util.UUID;
import java.util.function.Function;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.pkg.SourceFile;
import org.enso.text.Hex;

final class CacheUtils {
  private CacheUtils() {}

  private static int BUFFER_SIZE = 1024;

  static Function<Object, Object> writeReplace(CompilerContext context, boolean keepUUIDs) {
    return (obj) ->
        switch (obj) {
          case ProcessingPass.Metadata metadata -> metadata.prepareForSerialization(context);
          case UUID id -> keepUUIDs ? id : null;
          case null -> null;
          default -> obj;
        };
  }

  static Function<Object, Object> readResolve(CompilerContext context) {
    return (obj) ->
        switch (obj) {
          case ProcessingPass.Metadata metadata -> {
            var option = metadata.restoreFromSerialization(context);
            if (option.nonEmpty()) {
              yield option.get();
            } else {
              throw raise(RuntimeException.class, new IOException("Cannot convert " + metadata));
            }
          }
          case null -> null;
          default -> obj;
        };
  }

  /**
   * Returns a default hashing algorithm used for Enso caches.
   *
   * @return digest used for computing hashes
   */
  private static MessageDigest messageDigest() {
    try {
      return MessageDigest.getInstance("SHA-1");
    } catch (NoSuchAlgorithmException ex) {
      throw raise(RuntimeException.class, ex);
    }
  }

  /**
   * Computes digest from an array of bytes using a default hashing algorithm.
   *
   * @param bytes bytes for which hash will be computed
   * @return string representation of bytes' hash
   */
  static String computeDigestFromBytes(ByteBuffer bytes) {
    var sha = messageDigest();
    sha.update(bytes);
    return Hex.toHexString(sha.digest());
  }

  /**
   * Computes digest from package sources using a default hashing algorithm.
   *
   * @param pkgSources the list of package sources
   * @return string representation of bytes' hash
   */
  static final String computeDigestOfLibrarySources(List<SourceFile<TruffleFile>> pkgSources) {
    pkgSources.sort(Comparator.comparing(o -> o.qualifiedName().toString()));

    try {
      var digest = messageDigest();
      for (var source : pkgSources) {
        byte[] buffer = new byte[BUFFER_SIZE];
        try (InputStream is = source.file().newInputStream()) {
          int read = is.read(buffer, 0, BUFFER_SIZE);
          while (read > -1) {
            digest.update(buffer, 0, read);
            read = is.read(buffer, 0, BUFFER_SIZE);
          }
        }
      }
      return Hex.toHexString(digest.digest());
    } catch (IOException ex) {
      throw raise(RuntimeException.class, ex);
    }
  }

  @SuppressWarnings("unchecked")
  static <T extends Exception> T raise(Class<T> cls, Exception e) throws T {
    throw (T) e;
  }
}
