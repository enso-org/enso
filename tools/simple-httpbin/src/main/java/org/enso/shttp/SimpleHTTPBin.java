package org.enso.shttp;

import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.List;
import sun.misc.Signal;
import sun.misc.SignalHandler;

public class SimpleHTTPBin {

  private final HttpServer server;
  private final State state;

  public SimpleHTTPBin(String hostname, int port) throws IOException {
    InetSocketAddress address = new InetSocketAddress(hostname, port);
    server = HttpServer.create(address, 0);
    server.setExecutor(null);
    state = new State();
  }

  public void start() {
    server.start();
    state.start();

    try {
      while (state.isRunning()) {
        Thread.sleep(1000);
      }
    } catch (InterruptedException e) {
      e.printStackTrace();
    } finally {
      server.stop(0);
    }
  }

  public void stop() {
    state.stop();
  }

  public void addHandler(String path, HttpHandler handler) {
    server.createContext(path, handler);
  }

  public static void main(String[] args) {
    if (args.length != 2) {
      System.err.println("Usage: SimpleHTTPBin <host> <port>");
      System.exit(1);
    }

    String host = args[0];
    SimpleHTTPBin server = null;
    try {
      int port = Integer.valueOf(args[1]);
      server = new SimpleHTTPBin(host, port);
      for (HttpMethod method : HttpMethod.values()) {
        String path = "/" + method.toString().toLowerCase();
        server.addHandler(path, new TestHandler());
      }

      final SimpleHTTPBin server1 = server;
      SignalHandler stopServerHandler =
          (Signal sig) -> {
            System.out.println("Stopping server...");
            server1.stop();
          };
      for (String signalName : List.of("TERM", "INT")) {
        Signal.handle(new Signal(signalName), stopServerHandler);
      }
      server.start();
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      if (server != null) {
        server.stop();
      }
    }
  }

  private class State {
    private boolean running = false;

    void stop() {
      running = false;
    }

    void start() {
      running = true;
    }

    boolean isRunning() {
      return running;
    }
  }
}
