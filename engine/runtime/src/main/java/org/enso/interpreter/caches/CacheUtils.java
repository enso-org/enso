package org.enso.interpreter.caches;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.UUID;
import java.util.function.Function;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.editions.LibraryName;
import org.enso.persist.Persistance;
import org.enso.text.Hex;

final class CacheUtils {
  private CacheUtils() {}

  static Persistance.Pool createPool(CompilerContext context, boolean keepUUIDs) {
    return PersistUtils.POOL
        .withReadResolve(readResolve(context))
        .withWriteReplace(writeReplace(context, keepUUIDs));
  }

  private static Function<Object, Object> writeReplace(CompilerContext context, boolean keepUUIDs) {
    return (obj) ->
        switch (obj) {
          case ProcessingPass.Metadata metadata -> metadata.prepareForSerialization(context);
          case UUID id -> keepUUIDs ? id : null;
          case null -> null;
          default -> obj;
        };
  }

  private static Function<Object, Object> readResolve(CompilerContext context) {
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

  public static String computeDigestFromLibName(LibraryName libName) {
    var digest = messageDigest();
    digest.update(libName.qualifiedName().getBytes());
    return Hex.toHexString(digest.digest());
  }

  @SuppressWarnings("unchecked")
  static <T extends Exception> T raise(Class<T> cls, Exception e) throws T {
    throw (T) e;
  }
}
