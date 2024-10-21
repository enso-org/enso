package org.enso.aws;

import software.amazon.awssdk.auth.credentials.AwsCredentialsProvider;

import javax.crypto.Mac;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLParameters;
import java.io.IOException;
import java.net.Authenticator;
import java.net.CookieHandler;
import java.net.ProxySelector;
import java.net.URL;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/**
 * Makes HTTP requests with AWS signature v4.
 * Designed to be called by EnsoSecretHelper.makeRequest.
 */
public class SignedHttpClient extends HttpClient {
  private static final Logger LOGGER = Logger.getLogger("enso-aws-signer");

  private static final String SCHEME = "AWS4";
  private static final String ALGORITHM = "HMAC-SHA256";
  private static final String TERMINATOR = "aws4_request";

  private final String regionName;
  private final String serviceName;
  private final AwsCredentialsProvider credentialsProvider;
  private final HttpClient parent;
  private final String bodyHash;

  public SignedHttpClient(String regionName, String serviceName, AwsCredentialsProvider credentialsProvider, HttpClient parent, String bodyHash) {
    this.regionName = regionName;
    this.serviceName = serviceName;
    this.credentialsProvider = credentialsProvider;
    this.parent = parent;
    this.bodyHash = bodyHash;
  }

  @Override
  public Optional<CookieHandler> cookieHandler() {
    return parent.cookieHandler();
  }

  @Override
  public Optional<Duration> connectTimeout() {
    return parent.connectTimeout();
  }

  @Override
  public Redirect followRedirects() {
    return parent.followRedirects();
  }

  @Override
  public Optional<ProxySelector> proxy() {
    return parent.proxy();
  }

  @Override
  public SSLContext sslContext() {
    return parent.sslContext();
  }

  @Override
  public SSLParameters sslParameters() {
    return parent.sslParameters();
  }

  @Override
  public Optional<Authenticator> authenticator() {
    return parent.authenticator();
  }

  @Override
  public Version version() {
    return parent.version();
  }

  @Override
  public Optional<Executor> executor() {
    return parent.executor();
  }

  @Override
  public <T> CompletableFuture<HttpResponse<T>> sendAsync(HttpRequest request, HttpResponse.BodyHandler<T> responseBodyHandler) {
    throw new UnsupportedOperationException("Not implemented");
  }

  @Override
  public <T> CompletableFuture<HttpResponse<T>> sendAsync(HttpRequest request, HttpResponse.BodyHandler<T> responseBodyHandler, HttpResponse.PushPromiseHandler<T> pushPromiseHandler) {
    throw new UnsupportedOperationException("Not implemented");
  }

  @Override
  public <T> HttpResponse<T> send(HttpRequest request, HttpResponse.BodyHandler<T> responseBodyHandler) throws IOException, InterruptedException {
    URL url = request.uri().toURL();

    var headerMap = request.headers().map();
    var output = new HashMap<String, String>();

    var bodyPublisher = request.bodyPublisher().orElse(HttpRequest.BodyPublishers.noBody());
    long bodyLength = bodyPublisher.contentLength();
    output.put("content-length", bodyLength == 0 ? "" : Long.toString(bodyLength));

    output.put("x-amz-content-sha256", bodyHash);

    output.put("x-amz-date",
        ZonedDateTime.now(ZoneId.of("UTC")).format(DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")));

    int port = url.getPort();
    String hostHeader =  url.getHost() + (port == -1 ? "" : ":" + port);
    output.put("Host", hostHeader);

    // Create canonical headers
    var sortedHeaders = new ArrayList<String>();
    sortedHeaders.addAll(headerMap.keySet());
    sortedHeaders.addAll(output.keySet());
    sortedHeaders.sort(String.CASE_INSENSITIVE_ORDER);
    var canonicalHeaderNames = sortedHeaders.stream()
        .map(String::toLowerCase)
        .collect(Collectors.joining(";"));
    LOGGER.log(Level.WARNING, "Canonical header names: " + canonicalHeaderNames);
    var canonicalHeaders = sortedHeaders.stream()
        .map(k -> k.toLowerCase().replaceAll("\\s+", " ") + ":" + output.getOrDefault(k, headerMap.containsKey(k) ? headerMap.get(k).get(0) : null))
        .collect(Collectors.joining("\n"));
    LOGGER.log(Level.WARNING, "Canonical headers: " + canonicalHeaders);

    // Create canonical query string (not supported yet).
    if (url.getQuery() != null) {
      throw new UnsupportedOperationException("Query parameters are not supported yet.");
    }
    var queryParameters = "";

    // Create canonical request
    var canonicalPath = url.getPath();
    if (!canonicalPath.startsWith("/")) {
      canonicalPath = "/" + canonicalPath;
    }
    canonicalPath = URLEncoder.encode(canonicalPath, StandardCharsets.UTF_8).replace("%2F", "/");
    var canonicalRequest = String.join("\n", request.method(), canonicalPath, queryParameters, canonicalHeaders, "", canonicalHeaderNames, bodyHash);
    var canonicalRequestHash = getSHA256(canonicalRequest.getBytes(StandardCharsets.UTF_8));

    // Need the credentials
    var credentials = credentialsProvider.resolveCredentials();

    // Create signing string
    String dateStamp = LocalDate.now().format(DateTimeFormatter.BASIC_ISO_DATE);
    String scope = dateStamp + "/" + regionName + "/" + serviceName + "/" + TERMINATOR;
    String toSign = String.join("\n", SCHEME + "-" + ALGORITHM, output.get("x-amz-date"), scope, canonicalRequestHash);
    LOGGER.log(Level.WARNING, "String to sign: " + toSign);
    var signature = sign(SCHEME + credentials.secretAccessKey(), dateStamp, regionName, serviceName, TERMINATOR, toSign);

    // Build the authorization header
    var authorizationHeader = SCHEME + "-" + ALGORITHM + " "
        + "Credential=" + credentials.accessKeyId() + "/" + scope + ", "
        + "SignedHeaders=" + canonicalHeaderNames + ", "
        + "Signature=" + signature;
    output.put("Authorization", authorizationHeader);

    // Build a new request with the additional headers
    output.remove("Host");
    output.remove("content-length");
    var newBuilder = HttpRequest.newBuilder(request, (n, v) -> !output.containsKey(n));
    output.keySet().forEach(n -> newBuilder.header(n, output.get(n)));

    // Send the request
    return parent.send(newBuilder.build(), responseBodyHandler);
  }

  private static String sign(String init, String... values) {
    try {
      var mac = Mac.getInstance("HmacSHA256");
      byte[] key = init.getBytes(StandardCharsets.UTF_8);
      try {
        for (String value : values) {
          mac.init(new javax.crypto.spec.SecretKeySpec(key, ALGORITHM));
          key = mac.doFinal(value.getBytes(StandardCharsets.UTF_8));
        }
        return bytesToHex(key);
      } catch (java.security.InvalidKeyException e) {
        throw new RuntimeException("Failed to sign the request.", e);
      }
    } catch (NoSuchAlgorithmException e) {
      throw new RuntimeException("Failed to get HMAC-SHA-256 algorithm.", e);
    }
  }

  /**
   * Returns the SHA-256 hash of the given data.
   *
   * @param rawData the data to hash
   * @return the SHA-256 hash of the data
   */
  public static String getSHA256(byte[] rawData) {
    try {
      byte[] hash = MessageDigest.getInstance("SHA-256").digest(rawData);
      return bytesToHex(hash);
    } catch (NoSuchAlgorithmException e) {
      throw new RuntimeException("Failed to get SHA-256 algorithm.", e);
    }
  }

  private static String bytesToHex(byte[] hash) {
    StringBuilder hexString = new StringBuilder(2 * hash.length);
    for (byte b : hash) {
      String hex = Integer.toHexString(0xff & b);
      if (hex.length() == 1) {
        hexString.append('0');
      }
      hexString.append(hex);
    }
    return hexString.toString();
  }
}
