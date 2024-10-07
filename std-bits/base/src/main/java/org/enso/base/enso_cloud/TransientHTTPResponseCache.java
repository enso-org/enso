package org.enso.base.enso_cloud;

import java.io.IOException;
import java.net.http.HttpHeaders;
import java.net.URI;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.function.Supplier;
import java.util.List;
import org.graalvm.collections.Pair;

/**
 *
 */
public class TransientHTTPResponseCache {
  public TransientHTTPResponseCache() {
  }

  EnsoHttpResponse makeRequest(
      URI resolvedURI,
      List<Pair<String, String>> resolvedHeaders,
      //Supplier<EnsoHttpResponse> supplier)
      RequestMaker requestMaker)
      throws IOException, InterruptedException {
    var cacheKey = makeHashKey(resolvedURI, resolvedHeaders);
    var response = requestMaker.run();
    System.out.println("AAA h "+response.headers());
    System.out.println("AAA "+response.headers().firstValue("asdf"));
    Integer maxAge = getMaxAge(response.headers());
    System.out.println("AAA ma " + maxAge);
    return response;
  }

  private Integer getMaxAge(HttpHeaders headers) {
    var cacheControlMaybe = headers.firstValue("cache-control");
    Integer maxAge = null;
    if (cacheControlMaybe.isPresent()) {
      var cacheControl = cacheControlMaybe.get();
      var cacheControlEntries = cacheControl.split(",");
      for (var entry : cacheControlEntries) {
        if (entry.trim().toLowerCase().startsWith("max-age")) {
          var maxAgeBinding = entry.split("=");
          if (maxAgeBinding.length > 1) {
            maxAge = Integer.parseInt(maxAgeBinding[1]);
          }
          break;
        }
      }
    }
    return maxAge;
  }

  private String makeHashKey(
      URI resolvedURI,
      List<Pair<String, String>> resolvedHeaders) {
    try {
      MessageDigest messageDigest = MessageDigest.getInstance("SHA-256");
      System.out.println("AAA uri " + resolvedURI.toString());
      messageDigest.digest(resolvedURI.toString().getBytes());
      for (Pair<String, String> resolvedHeader : resolvedHeaders) {
        System.out.println("AAA header "+resolvedHeader.getLeft()+" "+resolvedHeader.getRight());
        messageDigest.digest(resolvedHeader.getLeft().getBytes());
        messageDigest.digest(resolvedHeader.getRight().getBytes());
      }
      return toHexString(messageDigest.digest());
    } catch (NoSuchAlgorithmException ex) {
      throw raise(RuntimeException.class, ex);
    }
  }

  public static String toHexString(byte[] bytes) {
    char[] out = new char[bytes.length * 2];
    for (int i = 0; i < bytes.length; i++) {
      int v = bytes[i] & 0xFF;
      out[i * 2] = Character.forDigit(v >>> 4, 16);
      out[i * 2 + 1] = Character.forDigit(v & 0x0F, 16);
    }
    return new String(out);
  }

  @FunctionalInterface
  public interface RequestMaker {
    EnsoHttpResponse run() throws IOException, InterruptedException;
  }

  @SuppressWarnings("unchecked")
  private static <E extends Exception> E raise(Class<E> type, Exception ex) throws E {
    throw (E) ex;
  }
}
