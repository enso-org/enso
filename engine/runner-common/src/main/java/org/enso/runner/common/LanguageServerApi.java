package org.enso.runner.common;

import java.util.ServiceLoader;
import org.apache.commons.cli.CommandLine;
import org.slf4j.event.Level;

public abstract class LanguageServerApi {
  public static final String DAEMONIZE_OPTION = "daemon";
  public static final String ROOT_ID_OPTION = "root-id";
  public static final String ROOT_PATH_OPTION = "path";
  public static final String INTERFACE_OPTION = "interface";
  public static final String RPC_PORT_OPTION = "rpc-port";
  public static final String DATA_PORT_OPTION = "data-port";
  public static final String SECURE_RPC_PORT_OPTION = "secure-rpc-port";
  public static final String SECURE_DATA_PORT_OPTION = "secure-data-port";
  public static final String SKIP_GRAALVM_UPDATER = "skip-graalvm-updater";

  public static void launchLanguageServer(CommandLine line, ProfilingConfig config, Level logLevel)
      throws WrongOption {
    var it =
        ServiceLoader.load(LanguageServerApi.class, LanguageServerApi.class.getClassLoader())
            .iterator();
    if (!it.hasNext()) {
      throw new WrongOption("No language server implementation found");
    }
    var impl = it.next();
    impl.runLanguageServer(line, config, logLevel);
  }

  protected abstract void runLanguageServer(
      CommandLine line, ProfilingConfig config, Level logLevel) throws WrongOption;
}
