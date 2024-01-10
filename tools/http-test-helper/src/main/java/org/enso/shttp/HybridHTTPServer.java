package org.enso.shttp;

import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import com.sun.net.httpserver.HttpsConfigurator;
import com.sun.net.httpserver.HttpsServer;

import javax.net.ssl.SSLContext;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.security.NoSuchAlgorithmException;

public class HybridHTTPServer {

  private final HttpServer server;
  private final HttpsServer sslServer;
  private volatile boolean isRunning;

  public HybridHTTPServer(String hostname, int port, int sslPort) throws IOException {
    InetSocketAddress address = new InetSocketAddress(hostname, port);
    server = HttpServer.create(address, 0);
    server.setExecutor(null);

    InetSocketAddress sslAddress = new InetSocketAddress(hostname, sslPort);
    sslServer = HttpsServer.create(sslAddress, 0);
    try {
      SSLContext context = SSLContext.getInstance("TLS");
      sslServer.setHttpsConfigurator(new HttpsConfigurator(context));
    } catch (NoSuchAlgorithmException e) {
      throw new RuntimeException(e);
    }
  }

  public void start() {
    server.start();
    sslServer.start();
    isRunning = true;

    System.out.println("HTTP server started at " + server.getAddress());
    System.out.println("HTTPS server started at " + sslServer.getAddress());

    try {
      while (isRunning) {
        Thread.sleep(500);
      }
    } catch (InterruptedException e) {
      e.printStackTrace();
    } finally {
      System.out.println("Finalizing HTTP server...");
      server.stop(1);
      System.out.println("Finalizing HTTPS server...");
      sslServer.stop(1);
      System.out.println("Server stopped.");
    }
  }

  public void stop() {
    isRunning = false;
  }

  public void addHandler(String path, HttpHandler handler) {
    server.createContext(path, handler);
    sslServer.createContext(path, handler);
  }
}
