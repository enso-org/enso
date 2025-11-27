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
  public static final String PROJECT_ID_OPTION = "project-id";
  public static final String CLOUD_PROJECT_ID_OPTION = "cloud-project-id";
  public static final String CLOUD_PROJECT_SESSION_ID_OPTION = "cloud-project-session-id";
  public static final String SECURE_RPC_PORT_OPTION = "secure-rpc-port";
  public static final String SECURE_DATA_PORT_OPTION = "secure-data-port";
  public static final String SKIP_GRAALVM_UPDATER = "skip-graalvm-updater";
  public static final String NO_LOG_MASKING_OPTION = "no-log-masking";

  public static final String ENSO_CLOUD_PROJECT_ID_ENV_NAME = "ENSO_CLOUD_PROJECT_ID";
  public static final String ENSO_CLOUD_PROJECT_SESSION_ID_ENV_NAME =
      "ENSO_CLOUD_PROJECT_SESSION_ID";

  public static void launchLanguageServer(CommandLine line, ProfilingConfig config, Level logLevel)
      throws WrongOption {
    var loader = LanguageServerApi.class.getClassLoader();
    var it = ServiceLoader.load(LanguageServerApi.class, loader).iterator();
    if (!it.hasNext()) {
      throw new WrongOption("No language server implementation found");
    }
    var impl = it.next();
    impl.runLanguageServer(line, config, logLevel);
  }

  protected abstract void runLanguageServer(
      CommandLine line, ProfilingConfig config, Level logLevel) throws WrongOption;
}
