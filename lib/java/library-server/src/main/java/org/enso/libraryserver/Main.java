package org.enso.libraryserver;

import java.nio.file.Path;
import java.util.concurrent.Semaphore;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

public final class Main {
  private static final String USAGE =
      "Usage: library-server [-v|--verbose] [-p|--port <port>] <directory>";

  // Includes time, logger name, log level, and message
  private static final String LOG_FORMAT = "[%1$tF %1$tT] [%4$-7s] [%3$s] %5$s %n";
  private static final String LOG_FORMAT_SYS_PROP = "java.util.logging.SimpleFormatter.format";

  public static void main(String[] args) {
    var verbose = false;
    var port = 8080;
    Path directory = null;

    for (var i = 0; i < args.length; i++) {
      var arg = args[i];
      if (arg.startsWith("-")) {
        switch (arg) {
          case "-v", "--verbose" -> verbose = true;
          case "-p", "--port" -> {
            i++;
            var portStr = args[i];
            port = Integer.parseInt(portStr);
          }
          case "-h", "--help" -> {
            System.out.println(USAGE);
            System.exit(0);
          }
        }
      } else {
        directory = Path.of(arg);
      }
    }

    if (directory == null) {
      System.err.println(USAGE);
      System.exit(1);
    }
    configureLogging(verbose);
    try (var server = new LibraryServer(port, directory)) {
      // Block until CTRL+C
      var semaphore = new Semaphore(0);
      Runtime.getRuntime().addShutdownHook(new Thread(semaphore::release));
      server.start();
      System.out.println("Server started on port " + port);
      try {
        semaphore.acquire();
        System.out.println("Shutting down...");
      } catch (InterruptedException e) {
        System.err.println("Shutting down abruptly...");
      }
    }
  }

  private static void configureLogging(boolean verbose) {
    System.setProperty(LOG_FORMAT_SYS_PROP, LOG_FORMAT);
    var consoleHandler = new ConsoleHandler();
    consoleHandler.setFormatter(new SimpleFormatter());
    var level = verbose ? Level.FINE : Level.INFO;
    consoleHandler.setLevel(level);

    var rootLogger = Logger.getLogger("");
    rootLogger.addHandler(consoleHandler);
  }
}
