package org.enso.jsonrpc;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.InetAddress;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.TimeUnit;
import javax.net.ssl.SSLContext;
import org.apache.commons.io.IOUtils;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.bootstrap.HttpServer;
import org.apache.http.impl.bootstrap.ServerBootstrap;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.BasicHttpClientConnectionManager;
import org.enso.jsonrpc.SecureConnectionConfig.SecureConnectionConfigForPublicPrivateCert;
import org.junit.After;
import org.junit.Test;

public class SSLContextBuilderTest {
  // See https://lightbend.github.io/ssl-config/CertificateGeneration.html or
  // https://docs.oracle.com/javase/8/docs/technotes/guides/security/jsse/JSSERefGuide.html#CreateKeystore
  // on how to generate test public/private certificates or pkcs12 files.

  HttpServer httpServer = null;

  @After
  public void stop() {
    if (httpServer != null) {
      httpServer.shutdown(0, TimeUnit.SECONDS);
      httpServer = null;
    }
  }

  private void testSSLContext(int port, SSLContext ctx) throws IOException {
    var sslsf = new SSLConnectionSocketFactory(ctx, NoopHostnameVerifier.INSTANCE);
    var socketFactoryRegistry =
        RegistryBuilder.<ConnectionSocketFactory>create()
            .register("https", sslsf)
            .register("http", new PlainConnectionSocketFactory())
            .build();

    var connectionManager = new BasicHttpClientConnectionManager(socketFactoryRegistry);
    var httpClient =
        HttpClients.custom()
            .setSSLSocketFactory(sslsf)
            .setConnectionManager(connectionManager)
            .build();
    httpServer =
        ServerBootstrap.bootstrap()
            .setLocalAddress(InetAddress.getByName("localhost"))
            .setListenerPort(port)
            .setSslContext(ctx)
            .setSslSetupHandler(socket -> socket.setNeedClientAuth(true))
            .registerHandler(
                "/*", (request, response, context) -> response.setStatusCode(HttpStatus.SC_OK))
            .create();
    httpServer.start();
    var request = new HttpGet("https://localhost:" + port);
    var response = httpClient.execute(request);
    assertEquals(response.getStatusLine().getStatusCode(), 200);
  }

  @Test
  public void testCreatingSSLContextFromPKCS12() throws IOException {
    var pcksFile = this.getClass().getResourceAsStream("/example.com.p12");
    assertNotNull(pcksFile);
    var password = "E4FtHvrLA4";
    var secureConnection =
        new SecureConnectionConfig.SecureConnectionConfigForPKCS12(pcksFile, password, true);
    var sslContext = secureConnection.generateSSLContext();
    assertTrue(sslContext.isSuccess());
    var ctx = sslContext.get();
    testSSLContext(8444, ctx);
  }

  @Test
  public void testCreatingSSLContextFromCertificate() throws IOException {
    var certFile = this.getClass().getResourceAsStream("/example.com.crt");
    var privateKey = this.getClass().getResourceAsStream("/example.com.key");
    assertNotNull(certFile);
    assertNotNull(privateKey);
    var secureConnection =
        new SecureConnectionConfigForPublicPrivateCert(
            IOUtils.toString(certFile, StandardCharsets.UTF_8),
            "X.509",
            IOUtils.toString(privateKey, StandardCharsets.UTF_8),
            true);
    var sslContext = secureConnection.generateSSLContext();
    assertTrue(sslContext.isSuccess());
    var ctx = sslContext.get();
    testSSLContext(8443, ctx);
  }
}
