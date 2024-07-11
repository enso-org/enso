package org.enso.languageserver.boot;

import java.util.UUID;
import org.apache.commons.cli.CommandLine;
import org.enso.runner.common.LanguageServerApi;
import org.enso.runner.common.WrongOption;
import org.openide.util.lookup.ServiceProvider;
import org.slf4j.event.Level;
import scala.concurrent.ExecutionContext;

@ServiceProvider(service = LanguageServerApi.class)
public final class LanguageServerRunner extends LanguageServerApi {
  public LanguageServerRunner() {}

  /**
   * Handles `--server` CLI option
   *
   * @param line a CLI line
   * @param logLevel log level to set for the engine runtime
   */
  protected final void runLanguageServer(CommandLine line, Level logLevel) {
    try {
      var config = parseServerOptions(line);
      LanguageServerApp.run(config, logLevel, line.hasOption(LanguageServerApi.DAEMONIZE_OPTION));
      throw exitSuccess();
    } catch (WrongOption e) {
      System.err.println(e.getMessage());
      throw exitFail();
    }
  }

  private static LanguageServerConfig parseServerOptions(CommandLine line) throws WrongOption {
    UUID rootId;
    try {
      var id = line.getOptionValue(LanguageServerApi.ROOT_ID_OPTION);
      if (id == null) {
        throw new WrongOption("Root id must be provided");
      }
      rootId = UUID.fromString(id);
    } catch (IllegalArgumentException e) {
      throw new WrongOption("Root must be UUID");
    }
    var rootPath = line.getOptionValue(LanguageServerApi.ROOT_PATH_OPTION);
    if (rootPath == null) {
      throw new WrongOption("Root path must be provided");
    }
    var interfac = line.getOptionValue(LanguageServerApi.INTERFACE_OPTION, "127.0.0.1");
    int rpcPort;
    try {
      rpcPort = Integer.parseInt(line.getOptionValue(LanguageServerApi.RPC_PORT_OPTION, "8080"));
    } catch (NumberFormatException e) {
      throw new WrongOption("Port must be integer");
    }
    int dataPort;
    try {
      dataPort = Integer.parseInt(line.getOptionValue(LanguageServerApi.DATA_PORT_OPTION, "8081"));
    } catch (NumberFormatException e) {
      throw new WrongOption("Port must be integer");
    }
    Integer secureRpcPort;
    try {
      var port = line.getOptionValue(LanguageServerApi.SECURE_RPC_PORT_OPTION);
      secureRpcPort = port == null ? null : Integer.valueOf(port);
    } catch (NumberFormatException e) {
      throw new WrongOption("Port must be integer");
    }
    Integer secureDataPort;
    try {
      var port = line.getOptionValue(LanguageServerApi.SECURE_DATA_PORT_OPTION);
      secureDataPort = port == null ? null : Integer.valueOf(port);
    } catch (NumberFormatException e) {
      throw new WrongOption("Port must be integer");
    }
    var profilingConfig = parseProfilingConfig(line);
    var graalVMUpdater = line.hasOption(LanguageServerApi.SKIP_GRAALVM_UPDATER);

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
