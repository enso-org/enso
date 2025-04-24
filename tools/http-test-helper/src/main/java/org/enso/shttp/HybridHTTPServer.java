package org.enso.shttp;

import com.sun.net.httpserver.*;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyStore;
import java.util.List;
import java.util.concurrent.Executor;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.TrustManagerFactory;
import org.enso.shttp.cloud_mock.CloudAuthRenew;
import org.enso.shttp.cloud_mock.CloudRoot;
import org.enso.shttp.cloud_mock.EventsService.LogEvent;

public class HybridHTTPServer {

  private final HttpServer server;
  private final HttpsServer sslServer;
  private final Path keyStorePath;
  private volatile boolean isStarted = false;
  private CloudRoot cloudRoot;
  private CloudAuthRenew cloudAuthRenew;

  HybridHTTPServer(
      String hostname,
      int port,
      int sslPort,
      Path keyStorePath,
      Executor executor,
      boolean withSSLServer)
      throws IOException {
    this.keyStorePath = keyStorePath;
    InetSocketAddress address = new InetSocketAddress(hostname, port);
    server = HttpServer.create(address, 0);
    server.setExecutor(executor);

    if (withSSLServer) {
      InetSocketAddress sslAddress = new InetSocketAddress(hostname, sslPort);
      sslServer = HttpsServer.create(sslAddress, 0);
      sslServer.setExecutor(executor);
    } else {
      sslServer = null;
    }
  }

  public List<LogEvent> getLogs() {
    return cloudRoot.getEvents();
  }

  /** Returns count successful requests for token refresh. */
  public int getRefreshedTokensCount() {
    return cloudAuthRenew.getRefreshedTokensCount();
  }

  private static class SimpleHttpsConfigurator extends HttpsConfigurator {
    public SimpleHttpsConfigurator(SSLContext context) {
      super(context);
    }

    @Override
    public void configure(HttpsParameters params) {
      SSLContext ctx = getSSLContext();
      SSLEngine engine = ctx.createSSLEngine();
      SSLParameters sslParams = ctx.getDefaultSSLParameters();
      sslParams.setNeedClientAuth(false);
      sslParams.setCipherSuites(engine.getEnabledCipherSuites());
      sslParams.setProtocols(engine.getEnabledProtocols());
      params.setSSLParameters(sslParams);
    }
  }

  private void setupSSL() throws Exception {
    String password = "test-password";
    SSLContext context = SSLContext.getInstance("TLS");
    KeyStore keyStore = initializeKeyStore(password);

    KeyManagerFactory keyManagerFactory =
        KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
    keyManagerFactory.init(keyStore, password.toCharArray());
    TrustManagerFactory trustManagerFactory =
        TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
    trustManagerFactory.init(keyStore);

    context.init(keyManagerFactory.getKeyManagers(), trustManagerFactory.getTrustManagers(), null);
    sslServer.setHttpsConfigurator(new SimpleHttpsConfigurator(context));
  }

  private KeyStore initializeKeyStore(String password) throws Exception {
    KeyStore keyStore = KeyStore.getInstance("JKS");

    if (Files.exists(keyStorePath)) {
      Files.delete(keyStorePath);
    }

    int exitCode =
        (new ProcessBuilder())
            .command(
                "keytool",
                "-genkey",
                "-alias",
                "test-key",
                "-keyalg",
                "RSA",
                "-keystore",
                keyStorePath.toAbsolutePath().normalize().toString(),
                "-storepass",
                password,
                "-keypass",
                password,
                "-dname",
                "CN=localhost",
                "-validity",
                "365",
                "-keysize",
                "2048")
            .inheritIO()
            .start()
            .waitFor();
    if (exitCode != 0) {
      throw new RuntimeException("Failed to generate keystore");
    }
    keyStore.load(Files.newInputStream(keyStorePath), password.toCharArray());
    return keyStore;
  }

  public void start() {
    if (isStarted) {
      throw new IllegalStateException("Server already started");
    }

    isStarted = true;

    if (sslServer != null) {
      try {
        setupSSL();
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    }
    server.start();
    if (sslServer != null) {
      sslServer.start();
    }

    System.out.println("HTTP server started at " + server.getAddress());
    if (sslServer != null) {
      System.out.println("HTTPS server started at " + sslServer.getAddress());
    }
  }

  public void stop() {
    System.out.println("Finalizing HTTP server...");
    server.stop(1);
    if (sslServer != null) {
      System.out.println("Finalizing HTTPS server...");
      sslServer.stop(1);
    }
  }

  public void addHandler(String path, HttpHandler handler) {
    server.createContext(path, handler);
    if (sslServer != null) {
      sslServer.createContext(path, handler);
    }
  }

  void addCloudRoot(CloudRoot cloudRoot) {
    this.cloudRoot = cloudRoot;
  }

  void addCloudAuthRenew(CloudAuthRenew cloudAuthRenew) {
    this.cloudAuthRenew = cloudAuthRenew;
  }
}
