package org.enso.languageserver.boot;

public final class LanguageServerRunner {
  public LanguageServerRunner() {}

  /**
   * Handles `--server` CLI option
   *
   * @param line a CLI line
   * @param logLevel log level to set for the engine runtime
   */
  private void runLanguageServer(CommandLine line, Level logLevel) {
    try {
      var config = parseServerOptions(line);
      LanguageServerApp.run(config, logLevel, line.hasOption(Main.DAEMONIZE_OPTION));
      throw exitSuccess();
    } catch (WrongOption e) {
      System.err.println(e.getMessage());
      throw exitFail();
    }
  }

  private static LanguageServerConfig parseServerOptions(CommandLine line) throws WrongOption {
    UUID rootId;
    try {
      var id = line.getOptionValue(ROOT_ID_OPTION);
      if (id == null) {
        throw new WrongOption("Root id must be provided");
      }
      rootId = UUID.fromString(id);
    } catch (IllegalArgumentException e) {
      throw new WrongOption("Root must be UUID");
    }
    var rootPath = line.getOptionValue(ROOT_PATH_OPTION);
    if (rootPath == null) {
      throw new WrongOption("Root path must be provided");
    }
    var interfac = line.getOptionValue(INTERFACE_OPTION, "127.0.0.1");
    int rpcPort;
    try {
      rpcPort = Integer.parseInt(line.getOptionValue(RPC_PORT_OPTION, "8080"));
    } catch (NumberFormatException e) {
      throw new WrongOption("Port must be integer");
    }
    int dataPort;
    try {
      dataPort = Integer.parseInt(line.getOptionValue(DATA_PORT_OPTION, "8081"));
    } catch (NumberFormatException e) {
      throw new WrongOption("Port must be integer");
    }
    Integer secureRpcPort;
    try {
      var port = line.getOptionValue(SECURE_RPC_PORT_OPTION);
      secureRpcPort = port == null ? null : Integer.valueOf(port);
    } catch (NumberFormatException e) {
      throw new WrongOption("Port must be integer");
    }
    Integer secureDataPort;
    try {
      var port = line.getOptionValue(SECURE_DATA_PORT_OPTION);
      secureDataPort = port == null ? null : Integer.valueOf(port);
    } catch (NumberFormatException e) {
      throw new WrongOption("Port must be integer");
    }
    var profilingConfig = parseProfilingConfig(line);
    var graalVMUpdater = line.hasOption(SKIP_GRAALVM_UPDATER);

    var config =
        new LanguageServerConfig(
            interfac,
            rpcPort,
            scala.Option.apply(secureRpcPort),
            dataPort,
            scala.Option.apply(secureDataPort),
            rootId,
            rootPath,
            profilingConfig,
            new StartupConfig(graalVMUpdater),
            "language-server",
            ExecutionContext.global());
    return config;
  }
}
