package org.enso.runner;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.enso.common.ContextFactory;
import org.enso.common.DebugServerInfo;
import org.enso.common.HostEnsoUtils;
import org.enso.common.LanguageInfo;
import org.enso.common.PythonHomeFinder;
import org.enso.distribution.DistributionManager;
import org.enso.distribution.Environment;
import org.enso.editions.DefaultEdition;
import org.enso.jvm.channel.JVM;
import org.enso.libraryupload.LibraryUploader.UploadFailedError;
import org.enso.logger.Converter;
import org.enso.logger.ObservedMessage;
import org.enso.pkg.Contact;
import org.enso.pkg.PackageManager;
import org.enso.pkg.PackageManager$;
import org.enso.pkg.Template;
import org.enso.polyglot.Module;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.debugger.DebuggerSessionManagerEndpoint;
import org.enso.profiling.sampler.MethodsSampler;
import org.enso.runner.common.LanguageServerApi;
import org.enso.runner.common.ProfilingConfig;
import org.enso.runner.common.WrongOption;
import org.enso.version.BuildVersion;
import org.enso.version.VersionDescription;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.PolyglotException.StackFrame;
import org.graalvm.polyglot.SourceSection;
import org.graalvm.polyglot.io.MessageTransport;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.slf4j.event.Level;
import scala.Option$;
import scala.concurrent.ExecutionContext;
import scala.concurrent.ExecutionContextExecutor;
import scala.runtime.BoxedUnit;

/** The main CLI entry point class. */
public class Main {
  private static final String JVM_OPTION = "jvm";
  private static final String RUN_OPTION = "run";
  private static final String INSPECT_OPTION = "inspect";
  private static final String HELP_OPTION = "help";
  private static final String NEW_OPTION = "new";
  private static final String PROJECT_NAME_OPTION = "new-project-name";
  private static final String PROJECT_NORMALIZED_NAME_OPTION = "new-project-normalized-name";
  private static final String PROJECT_TEMPLATE_OPTION = "new-project-template";
  private static final String PROJECT_AUTHOR_NAME_OPTION = "new-project-author-name";
  private static final String PROJECT_AUTHOR_EMAIL_OPTION = "new-project-author-email";
  private static final String REPL_OPTION = "repl";
  private static final String DOCS_OPTION = "docs";
  private static final String PREINSTALL_OPTION = "preinstall-dependencies";
  private static final String PROFILING_PATH = "profiling-path";
  private static final String PROFILING_TIME = "profiling-time";
  private static final String LANGUAGE_SERVER_OPTION = "server";
  private static final String LANGUAGE_SERVER_NATIVE_OPTION = "native-server";
  private static final String IN_PROJECT_OPTION = "in-project";
  private static final String VERSION_OPTION = "version";
  private static final String JSON_OPTION = "json";
  private static final String IR_CACHES_OPTION = "ir-caches";
  private static final String NO_IR_CACHES_OPTION = "no-ir-caches";
  private static final String NO_READ_IR_CACHES_OPTION = "no-read-ir-caches";
  private static final String DISABLE_PRIVATE_CHECK_OPTION = "disable-private-check";
  private static final String ENABLE_STATIC_ANALYSIS_OPTION = "enable-static-analysis";
  private static final String TREAT_WARNINGS_AS_ERRORS_OPTION = "Werror";
  private static final String COMPILE_OPTION = "compile";
  private static final String NO_COMPILE_DEPENDENCIES_OPTION = "no-compile-dependencies";
  private static final String LOG_LEVEL = "log-level";
  private static final String LOGGER_CONNECT = "logger-connect";
  private static final String NO_LOG_MASKING = "no-log-masking";
  private static final String UPLOAD_OPTION = "upload";
  private static final String HIDE_PROGRESS = "hide-progress";
  private static final String AUTH_TOKEN = "auth-token";
  private static final String AUTO_PARALLELISM_OPTION = "with-auto-parallelism";
  private static final String EXECUTION_ENVIRONMENT_OPTION = "execution-environment";
  private static final String WARNINGS_LIMIT = "warnings-limit";
  private static final String SYSTEM_PROPERTY = "vm.D";

  private static final String DEFAULT_MAIN_METHOD_NAME = "main";

  /** Value of this sys prop is comma-separated list of project paths. */
  private static final String DONT_CREATE_SRC_ARCHIVES_SYS_PROP =
      "org.enso.compiler.noSourceArchives";

  private static final Logger LOGGER = LoggerFactory.getLogger(Main.class);

  Main() {}

  private static boolean isDevBuild() {
    return BuildVersion.ensoVersion().matches(".+-SNAPSHOT$");
  }

  private static Option.Builder cliOptionBuilder() {
    return Option.builder();
  }

  private static final Options CLI_OPTIONS = buildOptions();

  private static Options buildOptions() {
    var help =
        cliOptionBuilder().option("h").longOpt(HELP_OPTION).desc("Displays this message.").build();
    var repl = cliOptionBuilder().longOpt(REPL_OPTION).desc("Runs the Enso REPL.").build();
    var run =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("file")
            .longOpt(RUN_OPTION)
            .desc("Runs a specified Enso file.")
            .build();
    var jvm =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .optionalArg(true)
            .argName("jvm")
            .longOpt(JVM_OPTION)
            .desc("Specifies whether to run JVM mode and optionally selects a JVM to run with.")
            .build();
    var inspect =
        cliOptionBuilder()
            .longOpt(INSPECT_OPTION)
            .desc("Start the Chrome inspector when --run is used.")
            .build();
    var docs =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .optionalArg(true)
            .longOpt(DOCS_OPTION)
            .desc(
                "Runs the Enso documentation generator. Additional argument may specify format -"
                    + " either the default `md` or `api`.")
            .build();
    var preinstall =
        cliOptionBuilder()
            .longOpt(PREINSTALL_OPTION)
            .desc("Installs dependencies of the project.")
            .build();
    var newOpt =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("path")
            .longOpt(NEW_OPTION)
            .desc("Creates a new Enso project.")
            .build();
    var newProjectNameOpt =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("name")
            .longOpt(PROJECT_NAME_OPTION)
            .desc("Specifies a project name when creating a project using --" + NEW_OPTION + ".")
            .build();
    var newProjectModuleNameOpt =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("name")
            .longOpt(PROJECT_NORMALIZED_NAME_OPTION)
            .desc(
                "Specifies a normalized (Upper_Snake_Case) name when creating a project using --"
                    + NEW_OPTION
                    + ".")
            .build();
    var newProjectTemplateOpt =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("name")
            .longOpt(PROJECT_TEMPLATE_OPTION)
            .desc(
                "Specifies a project template when creating a project using --" + NEW_OPTION + ".")
            .build();
    var newProjectAuthorNameOpt =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("name")
            .longOpt(PROJECT_AUTHOR_NAME_OPTION)
            .desc(
                "Specifies the name of the author and maintainer of the project "
                    + "created using --"
                    + NEW_OPTION
                    + ".")
            .build();
    var newProjectAuthorEmailOpt =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("email")
            .longOpt(PROJECT_AUTHOR_EMAIL_OPTION)
            .desc(
                "Specifies the email of the author and maintainer of the project "
                    + "created using --"
                    + NEW_OPTION
                    + ".")
            .build();
    var lsOption =
        cliOptionBuilder().longOpt(LANGUAGE_SERVER_OPTION).desc("Runs Language Server").build();
    var lsNativeOption =
        cliOptionBuilder()
            .longOpt(LANGUAGE_SERVER_NATIVE_OPTION)
            .desc("Runs Language Server in native-image mode")
            .build();
    var lsProfilingPathOption =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("file")
            .longOpt(PROFILING_PATH)
            .desc("The path to the profiling file.")
            .build();
    var lsProfilingTimeOption =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("seconds")
            .longOpt(PROFILING_TIME)
            .desc("The duration in seconds limiting the profiling time.")
            .build();
    var deamonizeOption =
        cliOptionBuilder()
            .longOpt(LanguageServerApi.DAEMONIZE_OPTION)
            .desc("Daemonize Language Server")
            .build();
    var interfaceOption =
        cliOptionBuilder()
            .longOpt(LanguageServerApi.INTERFACE_OPTION)
            .hasArg(true)
            .numberOfArgs(1)
            .argName("interface")
            .desc("Interface for processing all incoming connections")
            .build();
    var rpcPortOption =
        cliOptionBuilder()
            .longOpt(LanguageServerApi.RPC_PORT_OPTION)
            .hasArg(true)
            .numberOfArgs(1)
            .argName("rpc-port")
            .desc("RPC port for processing all incoming connections")
            .build();
    var secureRpcPortOption =
        cliOptionBuilder()
            .longOpt(LanguageServerApi.SECURE_RPC_PORT_OPTION)
            .hasArg(true)
            .numberOfArgs(1)
            .argName("rpc-port")
            .desc("A secure RPC port for processing all incoming connections")
            .build();
    var dataPortOption =
        cliOptionBuilder()
            .longOpt(LanguageServerApi.DATA_PORT_OPTION)
            .hasArg(true)
            .numberOfArgs(1)
            .argName("data-port")
            .desc("Data port for visualization protocol")
            .build();
    var secureDataPortOption =
        cliOptionBuilder()
            .longOpt(LanguageServerApi.SECURE_DATA_PORT_OPTION)
            .hasArg(true)
            .numberOfArgs(1)
            .argName("data-port")
            .desc("A secure data port for visualization protocol")
            .build();
    var uuidOption =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("uuid")
            .longOpt(LanguageServerApi.ROOT_ID_OPTION)
            .desc("Content root id.")
            .build();
    var projectIdOption =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("uuid")
            .longOpt(LanguageServerApi.PROJECT_ID_OPTION)
            .desc("Project id.")
            .build();
    var cloudProjectIdOption =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("id")
            .longOpt(LanguageServerApi.CLOUD_PROJECT_ID_OPTION)
            .desc("Cloud project id (hybrid).")
            .build();
    var cloudProjectSessionIdOption =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("id")
            .longOpt(LanguageServerApi.CLOUD_PROJECT_SESSION_ID_OPTION)
            .desc("Cloud project session id (hybrid).")
            .build();
    var pathOption =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("path")
            .longOpt(LanguageServerApi.ROOT_PATH_OPTION)
            .desc("Path to the content root.")
            .build();
    var inProjectOption =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("project-path")
            .longOpt(IN_PROJECT_OPTION)
            .desc(
                "Setting this option when running the REPL or an Enso script, runs it"
                    + "in context of the specified project.")
            .build();
    var version =
        cliOptionBuilder()
            .longOpt(VERSION_OPTION)
            .desc("Checks the version of the Enso executable.")
            .build();
    var json =
        cliOptionBuilder()
            .longOpt(JSON_OPTION)
            .desc("Switches the --version option to JSON output.")
            .build();
    var logLevelOption =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("log-level")
            .longOpt(LOG_LEVEL)
            .desc(
                "Sets the runtime log level. Possible values are: "
                    + getPossibleLogLevels()
                    + ". Default: info.")
            .build();
    var loggerConnectOption =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("uri")
            .longOpt(LOGGER_CONNECT)
            .desc("Connects to a logging service server and passes all logs to it.")
            .build();
    var noLogMaskingOption =
        cliOptionBuilder()
            .longOpt(NO_LOG_MASKING)
            .desc(
                "Disable masking of personally identifiable information in logs. "
                    + "Masking can be also disabled with the `NO_LOG_MASKING` environment "
                    + "variable.")
            .build();
    var uploadOption =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("url")
            .longOpt(UPLOAD_OPTION)
            .desc(
                "Uploads the library to a repository. "
                    + "The url defines the repository to upload to.")
            .build();
    var hideProgressOption =
        cliOptionBuilder()
            .longOpt(HIDE_PROGRESS)
            .desc("If specified, progress bars will not be displayed.")
            .build();
    var authTokenOption =
        cliOptionBuilder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("token")
            .longOpt(AUTH_TOKEN)
            .desc("Authentication token for the upload.")
            .build();
    var noReadIrCachesOption =
        cliOptionBuilder()
            .longOpt(NO_READ_IR_CACHES_OPTION)
            .desc("Disables the reading of IR caches in the runtime if IR caching is enabled.")
            .build();
    var compileOption =
        cliOptionBuilder()
            .longOpt(COMPILE_OPTION)
            .desc("Compile provided packages without executing.")
            .hasArgs()
            .argName("packages")
            .build();
    var noCompileDependenciesOption =
        cliOptionBuilder()
            .longOpt(NO_COMPILE_DEPENDENCIES_OPTION)
            .desc(
                "Tells the compiler to not compile dependencies when performing static"
                    + " compilation.")
            .build();

    var irCachesOption =
        cliOptionBuilder()
            .longOpt(IR_CACHES_OPTION)
            .desc(
                "Enables IR caches. These are on by default in production builds "
                    + "and off by default in developer builds. You may not specify this "
                    + "option with `--no-ir-caches`.")
            .build();
    var noIrCachesOption =
        cliOptionBuilder()
            .longOpt(NO_IR_CACHES_OPTION)
            .desc(
                "Disables IR caches. These are on by default in production builds "
                    + "and off by default in developer builds. You may not specify this "
                    + "option with `--ir-caches`.")
            .build();

    var cacheOptionsGroup = new OptionGroup();
    cacheOptionsGroup.addOption(irCachesOption);
    cacheOptionsGroup.addOption(noIrCachesOption);

    var autoParallelism =
        cliOptionBuilder()
            .longOpt(AUTO_PARALLELISM_OPTION)
            .desc("Enables auto parallelism in the Enso interpreter.")
            .build();

    var skipGraalVMUpdater =
        cliOptionBuilder()
            .longOpt(LanguageServerApi.SKIP_GRAALVM_UPDATER)
            .desc("Skips GraalVM and its components setup during bootstrapping.")
            .build();

    var executionEnvironmentOption =
        cliOptionBuilder()
            .longOpt(EXECUTION_ENVIRONMENT_OPTION)
            .hasArg(true)
            .numberOfArgs(1)
            .argName("name")
            .desc(
                "Execution environment to use during execution (`live`/`design`). Defaults to"
                    + " `live`.")
            .build();

    var warningsLimitOption =
        cliOptionBuilder()
            .longOpt(WARNINGS_LIMIT)
            .hasArg(true)
            .numberOfArgs(1)
            .argName("limit")
            .desc("Specifies a maximal number of reported warnings. Defaults to `100`.")
            .build();

    var disablePrivateCheckOption =
        cliOptionBuilder()
            .longOpt(DISABLE_PRIVATE_CHECK_OPTION)
            .desc("Disables private module checking at runtime. Useful for tests.")
            .build();
    var enableStaticAnalysisOption =
        cliOptionBuilder()
            .longOpt(ENABLE_STATIC_ANALYSIS_OPTION)
            .desc("Enable static analysis (Experimental type inference).")
            .build();
    var treatWarningsAsErrorsOption =
        cliOptionBuilder()
            .option(TREAT_WARNINGS_AS_ERRORS_OPTION)
            .desc("Treat compiler warnings as errors.")
            .build();

    var systemPropOption =
        cliOptionBuilder()
            .longOpt(SYSTEM_PROPERTY)
            .argName("<property>=<value>")
            .desc(
                "Sets a system property. May be specified multiple times. If `value` is not"
                    + " specified, 'true' is inserted.")
            .hasArg(true)
            .numberOfArgs(1)
            .build();

    var options = new Options();
    options
        .addOption(help)
        .addOption(repl)
        .addOption(jvm)
        .addOption(run)
        .addOption(inspect)
        .addOption(docs)
        .addOption(preinstall)
        .addOption(newOpt)
        .addOption(newProjectNameOpt)
        .addOption(newProjectModuleNameOpt)
        .addOption(newProjectTemplateOpt)
        .addOption(newProjectAuthorNameOpt)
        .addOption(newProjectAuthorEmailOpt)
        .addOption(lsOption)
        .addOption(lsNativeOption)
        .addOption(lsProfilingPathOption)
        .addOption(lsProfilingTimeOption)
        .addOption(deamonizeOption)
        .addOption(interfaceOption)
        .addOption(rpcPortOption)
        .addOption(dataPortOption)
        .addOption(secureRpcPortOption)
        .addOption(secureDataPortOption)
        .addOption(uuidOption)
        .addOption(projectIdOption)
        .addOption(cloudProjectIdOption)
        .addOption(cloudProjectSessionIdOption)
        .addOption(pathOption)
        .addOption(inProjectOption)
        .addOption(version)
        .addOption(json)
        .addOption(logLevelOption)
        .addOption(loggerConnectOption)
        .addOption(noLogMaskingOption)
        .addOption(uploadOption)
        .addOption(hideProgressOption)
        .addOption(authTokenOption)
        .addOption(noReadIrCachesOption)
        .addOption(compileOption)
        .addOption(noCompileDependenciesOption)
        .addOptionGroup(cacheOptionsGroup)
        .addOption(autoParallelism)
        .addOption(skipGraalVMUpdater)
        .addOption(executionEnvironmentOption)
        .addOption(warningsLimitOption)
        .addOption(disablePrivateCheckOption)
        .addOption(systemPropOption)
        .addOption(enableStaticAnalysisOption)
        .addOption(treatWarningsAsErrorsOption);

    return options;
  }

  /** Prints the help message to the standard output. */
  void printHelp() {
    new HelpFormatter().printHelp(LanguageInfo.ID, CLI_OPTIONS);
  }

  /**
   * Terminates the process with a failure exit code.
   *
   * @param error the error message to send to stderr before terminating the process, can be {@code
   *     null}
   */
  private RuntimeException exitFail(String error) {
    if (error != null) {
      stderr(error);
    }
    return doExit(1);
  }

  /** Terminates the process with a success exit code. */
  private final RuntimeException exitSuccess() {
    return doExit(0);
  }

  /** Shuts down the logging service and terminates the process. */
  RuntimeException doExit(int exitCode) {
    RunnerLogging.tearDown();
    System.exit(exitCode);
    return null;
  }

  /**
   * Handles the `--new` CLI option.
   *
   * <p>Creates a project at the provided path. If the nameOption is provided it specifies the
   * project name, otherwise the name is generated automatically. The Enso version used in the
   * project is set to the version of this runner.
   *
   * @param path root path of the newly created project
   * @param nameOption specifies the name of the created project
   * @param normalizedNameOption specifies the normalized name of the created project
   * @param templateOption specifies the template of the created project
   * @param authorName if set, sets the name of the author and maintainer
   * @param authorEmail if set, sets the email of the author and maintainer
   */
  private void createNew(
      String path,
      scala.Option<String> nameOption,
      scala.Option<String> normalizedNameOption,
      scala.Option<String> templateOption,
      scala.Option<String> authorName,
      scala.Option<String> authorEmail) {
    final var root = new File(path);
    String name = nameOption.getOrElse(() -> PackageManager$.MODULE$.Default().generateName(root));
    scala.collection.immutable.List<Contact> authors =
        (authorName.isEmpty() && authorEmail.isEmpty())
            ? nil()
            : join(new Contact(authorName, authorEmail), nil());

    var edition = DefaultEdition.getDefaultEdition();
    if (LOGGER.isTraceEnabled()) {
      var baseEdition = edition.parent().getOrElse(() -> "<no-base>");
      LOGGER.trace("Creating a new project " + name + " based on edition [" + baseEdition + "].");
    }

    var template =
        templateOption.map(
            (n) ->
                Template.fromString(n)
                    .getOrElse(
                        () -> {
                          throw exitFail("Unknown project template name: '" + n + "'.");
                        }));

    PackageManager$.MODULE$
        .Default()
        .create(
            root,
            name,
            "local",
            normalizedNameOption,
            "0.0.1",
            template.getOrElse(() -> Template.Default$.MODULE$),
            scala.Option.apply(edition),
            authors,
            nil(),
            "",
            Option$.MODULE$.empty(),
            nil(),
            Option$.MODULE$.empty(),
            false);
    throw exitSuccess();
  }

  /**
   * Handles the `--compile` CLI option.
   *
   * @param paths Path of packages to be compiled.
   * @param shouldCompileDependencies whether the dependencies of that package should also be
   *     compiled
   * @param shouldUseIrCaches whether or not IR caches should be used.
   * @param disablePrivateCheck whether or not the private check should be disabled
   * @param enableStaticAnalysis whether or not static type checking, and other static analysis,
   *     should be enabled
   * @param treatWarningsAsErrors whether or not warnings should be treated as errors
   * @param logLevel the logging level
   * @param logMasking whether or not log masking is enabled
   */
  private void compile(
      String cwd,
      String[] paths,
      boolean shouldCompileDependencies,
      boolean shouldUseIrCaches,
      boolean disablePrivateCheck,
      boolean enableStaticAnalysis,
      boolean treatWarningsAsErrors,
      boolean showProgress,
      Level logLevel,
      boolean logMasking)
      throws IOException {
    var mainProjectPath = paths[0];
    var fileAndProject = Utils.findFileAndProject(cwd, mainProjectPath, null);
    assert fileAndProject != null;

    boolean isProjectMode = fileAndProject._1();
    String projectPath = fileAndProject._3();
    var context =
        new PolyglotContext(
            ContextFactory.create()
                .projectRoot(projectPath)
                .in(System.in)
                .out(System.out)
                .logLevel(Converter.toJavaLevel(logLevel))
                .logMasking(logMasking)
                .enableIrCaches(shouldUseIrCaches)
                .disablePrivateCheck(disablePrivateCheck)
                .enableStaticAnalysis(enableStaticAnalysis)
                .treatWarningsAsErrors(treatWarningsAsErrors)
                .strictErrors(true)
                .build());

    try {
      if (isProjectMode) {
        var topScope = context.getTopScope();
        topScope.compile(shouldCompileDependencies, paths);
        for (var path : paths) {
          updateManifestAndCreateArchive(path, logLevel, showProgress);
        }
      } else {
        context.evalModule(fileAndProject._2());
      }
      throw exitSuccess();
    } catch (Throwable t) {
      boolean compilationFailed =
          t instanceof PolyglotException polyglotException && polyglotException.isSyntaxError();
      if (compilationFailed) {
        var reason = treatWarningsAsErrors ? "warnings or errors" : "errors";
        throw exitFail("Compilation failed due to " + reason + ".");
      } else {
        String message = "Unexpected internal error: " + t.getMessage();
        LOGGER.error(message, t);
        throw exitFail(message);
      }

    } finally {
      context.context().close();
    }
  }

  /**
   * Updates the manifest of the project specified by its path and maybe creates a source archive.
   */
  private static void updateManifestAndCreateArchive(
      String path, Level logLevel, boolean showProgress) {
    var shouldCreateArchive = shouldCreateSourceArchiveForProject(path);
    var p = Path.of(path);
    ProjectUploader.updateManifest(p, logLevel, shouldCreateArchive);
    if (shouldCreateArchive) {
      ProjectUploader.createSourceArchive(p, logLevel, showProgress);
    }
  }

  private static boolean shouldCreateSourceArchiveForProject(String projPath) {
    var prop = System.getProperty(DONT_CREATE_SRC_ARCHIVES_SYS_PROP);
    if (prop == null) {
      return true;
    }
    var paths = prop.split(",");
    for (var p : paths) {
      if (p.equals(projPath)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Handles the `--run` CLI option.
   *
   * <p>If `path` is a directory, so a project is run, a conflicting (pointing to another project)
   * `projectPath` should not be provided.
   *
   * @param path path of the project or file to execute
   * @param projectPath if specified, the script is run in context of a project located at that path
   * @param logLevel log level to set for the engine runtime
   * @param logMasking is the log masking enabled
   * @param enableIrCaches are IR caches enabled
   * @param disablePrivateCheck Is private modules check disabled. If yes, `private` keyword is
   *     ignored.
   * @param enableStaticAnalysis whether or not static type checking should be enabled
   * @param treatWarningsAsErrors whether or not warnings should be treated as errors
   * @param inspect shall inspect option be enabled
   * @param executionEnvironment name of the execution environment to use during execution or {@code
   *     null}
   */
  private void handleRun(
      String cwd,
      String path,
      List<String> additionalArgs,
      String projectPath,
      Level logLevel,
      boolean logMasking,
      boolean enableIrCaches,
      boolean disablePrivateCheck,
      boolean enableAutoParallelism,
      boolean enableStaticAnalysis,
      boolean treatWarningsAsErrors,
      boolean enableDebugServer,
      boolean inspect,
      String executionEnvironment,
      int warningsLimit)
      throws IOException {
    var fileAndProject = Utils.findFileAndProject(cwd, path, projectPath);
    assert fileAndProject != null;
    var projectMode = fileAndProject._1();
    var file = fileAndProject._2();
    var mainFile = file;
    if (projectMode) {
      var result = PackageManager$.MODULE$.Default().loadPackage(file);
      if (result.isSuccess()) {
        var pkg = result.get();

        mainFile = pkg.mainFile();
        if (!mainFile.exists()) {
          throw exitFail("Main file does not exist.");
        }
      } else {
        throw exitFail(result.failed().get().getMessage());
      }
    }

    var projectRoot = fileAndProject._3();
    var options = new HashMap<String, String>();

    String pythonResourceDir = null;
    if (PythonHomeFinder.findPythonHome() instanceof Path pythonHome) {
      pythonResourceDir = pythonHome.getParent().toFile().getCanonicalPath();
    }

    var factory =
        ContextFactory.create()
            .projectRoot(projectRoot)
            .logLevel(Converter.toJavaLevel(logLevel))
            .logMasking(logMasking)
            .enableIrCaches(enableIrCaches)
            .disablePrivateCheck(disablePrivateCheck)
            .pythonResourceDir(pythonResourceDir)
            .strictErrors(true)
            .enableAutoParallelism(enableAutoParallelism)
            .enableStaticAnalysis(enableStaticAnalysis)
            .treatWarningsAsErrors(treatWarningsAsErrors)
            .executionEnvironment(executionEnvironment != null ? executionEnvironment : "live")
            .warningsLimit(warningsLimit)
            .options(options);

    if (inspect) {
      if (enableDebugServer) {
        throw exitFail("Cannot use --inspect and --repl and --run at once");
      }
      options.put("inspect", "");
    }
    if (enableDebugServer) {
      factory.messageTransport(replTransport());
      factory.enableDebugServer(true);
    } else {
      factory.checkForWarnings(mainFile.getName().replace(".enso", "") + ".main");
    }
    var context = new PolyglotContext(factory.build());
    try {
      if (projectMode) {
        var result = PackageManager$.MODULE$.Default().loadPackage(file);
        if (result.isSuccess()) {
          var pkg = result.get();
          var mainModuleName = pkg.moduleNameForFile(pkg.mainFile()).toString();
          runPackage(context, mainModuleName, file, additionalArgs);
        } else {
          throw exitFail(result.failed().get().getMessage());
        }
      } else {
        runSingleFile(context, file, additionalArgs);
      }
    } catch (RuntimeException e) {
      // forces computation of the exception message sooner than context is closed
      // should work around issues seen at #11127
      LOGGER.debug("Execution failed with " + e.getMessage());
      throw e;
    } finally {
      context.context().close();
    }
    throw exitSuccess();
  }

  /**
   * Handles the `--docs` CLI option.
   *
   * <p>Generates reference website from standard library.
   *
   * @param projectPath if specified, the docs is generated for a project at the given path
   * @param logLevel log level to set for the engine runtime
   * @param logMasking is the log masking enabled
   * @param enableIrCaches are the IR caches enabled
   */
  private void genDocs(
      String docsFormat,
      String cwd,
      String projectPath,
      Level logLevel,
      boolean logMasking,
      boolean enableIrCaches)
      throws IOException {
    if (projectPath == null || projectPath.isEmpty()) {
      throw exitFail("Specify path to a project with --in-project option");
    }
    var fileAndProject = Utils.findFileAndProject(cwd, projectPath, null);
    if (fileAndProject == null) {
      throw exitFail("Project specified in --in-project option does not exist: " + projectPath);
    }
    generateDocsFrom(docsFormat, projectPath, logLevel, logMasking, enableIrCaches);
    throw exitSuccess();
  }

  /**
   * Subroutine of `genDocs` function. Generates the documentation for given Enso project at given
   * path.
   */
  private void generateDocsFrom(
      String docsFormat, String path, Level logLevel, boolean logMasking, boolean enableIrCaches) {
    var executionContext =
        new PolyglotContext(
            ContextFactory.create()
                .projectRoot(path)
                .in(System.in)
                .out(System.out)
                .logLevel(Converter.toJavaLevel(logLevel))
                .logMasking(logMasking)
                .enableIrCaches(enableIrCaches)
                .build());

    var file = new File(path);
    var pkg = PackageManager.Default().fromDirectory(file);
    var main = pkg.map(x -> x.mainFile());

    if (main.exists(x -> x.exists())) {
      var topScope = executionContext.getTopScope();
      topScope.compile(false, scala.Option.apply(docsFormat == null ? "md" : docsFormat));
    }
  }

  /**
   * Handles the `--preinstall-dependencies` CLI option.
   *
   * <p>Gathers imported dependencies and ensures that all of them are installed.
   *
   * @param projectPath path of the project
   * @param logLevel log level to set for the engine runtime
   */
  private void preinstallDependencies(String projectPath, Level logLevel) {
    if (projectPath == null) {
      throw exitFail("Dependency installation is only available for projects.");
    }
    try {
      DependencyPreinstaller.preinstallDependencies(new File(projectPath), logLevel);
      throw exitSuccess();
    } catch (RuntimeException error) {
      LOGGER.error("Dependency installation failed: " + error.getMessage(), error);
      throw exitFail("Dependency installation failed: " + error.getMessage());
    }
  }

  private void runPackage(
      PolyglotContext context,
      String mainModuleName,
      File projectPath,
      java.util.List<String> additionalArgs) {
    var topScope = context.getTopScope();
    var mainModule = topScope.getModule(mainModuleName);
    runMain(mainModule, projectPath, additionalArgs, DEFAULT_MAIN_METHOD_NAME);
  }

  private void runSingleFile(
      PolyglotContext context, File file, java.util.List<String> additionalArgs) {
    var mainModule = context.evalModule(file);
    runMain(mainModule, file, additionalArgs, DEFAULT_MAIN_METHOD_NAME);
  }

  private void runMain(
      Module mainModule,
      File rootPkgPath,
      java.util.List<String> additionalArgs,
      String mainMethodName // = DEFAULT_MAIN_METHOD_NAME
      ) {
    try {
      var mainType = mainModule.getAssociatedType();
      var mainFun = mainModule.getMethod(mainType, mainMethodName);
      if (mainFun.isEmpty()) {
        throw exitFail(
            "The module "
                + mainModule.getName()
                + " does not contain a `main` "
                + "function. It could not be run.");
      }
      var main = mainFun.get();
      if (!DEFAULT_MAIN_METHOD_NAME.equals(mainMethodName)) {
        main.execute(join(mainType, nil()));
      } else {
        // Opportunistically parse arguments and convert to ints.
        // This avoids conversions in main function.
        var listOfArgs = nil();
        for (var arg : additionalArgs) {
          Object e;
          try {
            e = Integer.valueOf(arg);
          } catch (NumberFormatException ex) {
            e = arg;
          }
          listOfArgs = join(e, listOfArgs);
        }
        listOfArgs = listOfArgs.reverse();
        LOGGER.debug("Executing the main function with arguments {}", listOfArgs.mkString(", "));
        var res = main.execute(listOfArgs);
        if (!res.isNull()) {
          var textRes = res.isString() ? res.asString() : res.toString();
          stdout(textRes);
          if (res.isException()) {
            try {
              throw res.throwException();
            } catch (PolyglotException e) {
              if (e.isExit()) {
                throw doExit(e.getExitStatus());
              }
            }
          }
        }
      }
    } catch (PolyglotException e) {
      if (e.isExit()) {
        throw doExit(e.getExitStatus());
      } else {
        printPolyglotException(e, rootPkgPath);
        throw exitFail(e.getMessage());
      }
    }
  }

  /**
   * Handles the `--repl` CLI option
   *
   * @param projectPath if specified, the REPL is run in context of a project at the given path
   * @param logLevel log level to set for the engine runtime
   * @param logMasking is the log masking enabled
   * @param enableIrCaches are IR caches enabled
   * @param enableStaticAnalysis whether or not static type checking should be enabled
   * @param treatWarningsAsErrors whether or not warnings should be treated as errors
   */
  private void runRepl(
      String projectPath,
      Level logLevel,
      boolean logMasking,
      boolean enableIrCaches,
      boolean enableStaticAnalysis,
      boolean treatWarningsAsErrors) {
    var mainMethodName = "internal_repl_entry_point___";
    var dummySourceToTriggerRepl =
        """
        from Standard.Base import all
        import Standard.Base.Runtime.Debug

        $mainMethodName = Debug.breakpoint
        """
            .replace("$mainMethodName", mainMethodName);
    var replModuleName = "Internal_Repl_Module___";
    var projectRoot = projectPath != null ? projectPath : "";

    var context =
        new PolyglotContext(
            ContextFactory.create()
                .projectRoot(projectRoot)
                .messageTransport(replTransport())
                .enableDebugServer(true)
                .logLevel(Converter.toJavaLevel(logLevel))
                .executionEnvironment("live")
                .logMasking(logMasking)
                .enableIrCaches(enableIrCaches)
                .disableLinting(true)
                .enableStaticAnalysis(enableStaticAnalysis)
                .treatWarningsAsErrors(treatWarningsAsErrors)
                .build());
    var mainModule = context.evalModule(dummySourceToTriggerRepl, replModuleName);
    runMain(mainModule, null, Collections.emptyList(), mainMethodName);
    throw exitSuccess();
  }

  private static MessageTransport replTransport() {
    ThreadFactory factory = (r) -> new Thread(r, "Initialize Enso Terminal");
    var executor = Executors.newSingleThreadExecutor(factory);
    var futureRepl = executor.submit(() -> new Repl(makeTerminalForRepl()));
    MessageTransport transport =
        (uri, peer) -> {
          if (DebugServerInfo.URI.equals(uri.toString())) {
            try {
              var repl = futureRepl.get();
              return new DebuggerSessionManagerEndpoint(repl, peer);
            } catch (InterruptedException | ExecutionException ex) {
              LOGGER.error("Cannot initialize REPL transport", ex);
            }
          }
          return null;
        };
    return transport;
  }

  /**
   * Prints the version of the Enso executable.
   *
   * @param useJson whether the output should be JSON or human-readable.
   */
  private void displayVersion(boolean useJson) {
    var customVersion = CurrentVersion.getVersion().toString();
    var versionDescription =
        VersionDescription.make("Enso Compiler and Runtime", true, false, List.of(), customVersion);
    stdout(versionDescription.asString(useJson));
  }

  /** Parses the log level option. */
  private Level parseLogLevel(String levelOption) {
    var name = levelOption.toLowerCase();
    var found =
        Stream.of(Level.values()).filter(x -> name.equals(x.name().toLowerCase())).findFirst();
    if (found.isEmpty()) {
      throw exitFail("Invalid log level. Possible values are " + getPossibleLogLevels() + ".");
    } else {
      return found.get();
    }
  }

  private static String getPossibleLogLevels() {
    return Stream.of(Level.values())
        .map(x -> x.toString().toLowerCase())
        .collect(Collectors.joining(", "));
  }

  /** Parses an URI that specifies the logging service connection. */
  private URI parseUri(String string) {
    try {
      return new URI(string);
    } catch (URISyntaxException ex) {
      throw exitFail("`" + string + "` is not a valid URI.");
    }
  }

  /** Default log level to use if the LOG_LEVEL option is not provided. */
  private static final Level defaultLogLevel = Level.WARN;

  /**
   * Main entry point for the CLI program.
   *
   * @param args the command line arguments
   */
  public static void main(String[] args) throws Exception {
    new Main().launch(args);
  }

  /**
   * Main entry point for the CLI program.
   *
   * @param cwd current working directory to use
   * @param line the provided command line arguments
   * @param logLevel the provided log level
   * @param logMasking the flag indicating if the log masking is enabled
   */
  final void mainEntry(String cwd, CommandLine line, Level logLevel, boolean logMasking)
      throws IOException {
    if (line.hasOption(HELP_OPTION)) {
      printHelp();
      throw exitSuccess();
    }
    if (line.hasOption(VERSION_OPTION)) {
      displayVersion(line.hasOption(JSON_OPTION));
      throw exitSuccess();
    }

    if (line.hasOption(NEW_OPTION)) {
      createNew(
          line.getOptionValue(NEW_OPTION),
          scala.Option.apply(line.getOptionValue(PROJECT_NAME_OPTION)),
          scala.Option.apply(line.getOptionValue(PROJECT_NORMALIZED_NAME_OPTION)),
          scala.Option.apply(line.getOptionValue(PROJECT_TEMPLATE_OPTION)),
          scala.Option.apply(line.getOptionValue(PROJECT_AUTHOR_NAME_OPTION)),
          scala.Option.apply(line.getOptionValue(PROJECT_AUTHOR_EMAIL_OPTION)));
    }

    if (line.hasOption(UPLOAD_OPTION)) {
      scala.Option<Path> projectRoot =
          scala.Option.apply(line.getOptionValue(IN_PROJECT_OPTION))
              .map(x -> Path.of(x))
              .getOrElse(
                  () -> {
                    throw exitFail(
                        "When uploading, the "
                            + IN_PROJECT_OPTION
                            + " is mandatory "
                            + "to specify which project to upload.");
                  });

      try {
        ProjectUploader.uploadProject(
            projectRoot.get(),
            line.getOptionValue(UPLOAD_OPTION),
            line.getOptionValue(AUTH_TOKEN),
            !line.hasOption(HIDE_PROGRESS),
            logLevel);
        throw exitSuccess();
      } catch (UploadFailedError ex) {
        // We catch this error to avoid printing an unnecessary stack trace.
        // The error itself is already logged.
        throw exitFail(ex.getMessage());
      }
    }

    if (line.hasOption(COMPILE_OPTION)) {
      var packagePaths = line.getOptionValues(COMPILE_OPTION);
      var shouldCompileDependencies = !line.hasOption(NO_COMPILE_DEPENDENCIES_OPTION);

      compile(
          cwd,
          packagePaths,
          shouldCompileDependencies,
          shouldEnableIrCaches(line, null),
          line.hasOption(DISABLE_PRIVATE_CHECK_OPTION),
          line.hasOption(ENABLE_STATIC_ANALYSIS_OPTION),
          line.hasOption(TREAT_WARNINGS_AS_ERRORS_OPTION),
          !line.hasOption(HIDE_PROGRESS),
          logLevel,
          logMasking);
    }

    LOGGER.debug("Original working directory={}, cwd={}", cwd, System.getProperty("user.dir"));
    if (line.hasOption(RUN_OPTION)) {
      handleRun(
          cwd,
          line.getOptionValue(RUN_OPTION),
          Arrays.asList(line.getArgs()),
          line.getOptionValue(IN_PROJECT_OPTION),
          logLevel,
          logMasking,
          shouldEnableIrCaches(line, null),
          line.hasOption(DISABLE_PRIVATE_CHECK_OPTION),
          line.hasOption(AUTO_PARALLELISM_OPTION),
          line.hasOption(ENABLE_STATIC_ANALYSIS_OPTION),
          line.hasOption(TREAT_WARNINGS_AS_ERRORS_OPTION),
          line.hasOption(REPL_OPTION),
          line.hasOption(INSPECT_OPTION),
          line.getOptionValue(EXECUTION_ENVIRONMENT_OPTION),
          scala.Option.apply(line.getOptionValue(WARNINGS_LIMIT))
              .map(Integer::parseInt)
              .getOrElse(() -> 100));
    }
    if (line.hasOption(REPL_OPTION) && !line.hasOption(RUN_OPTION)) {
      runRepl(
          line.getOptionValue(IN_PROJECT_OPTION),
          logLevel,
          logMasking,
          shouldEnableIrCaches(line, null),
          line.hasOption(ENABLE_STATIC_ANALYSIS_OPTION),
          line.hasOption(TREAT_WARNINGS_AS_ERRORS_OPTION));
    }
    if (line.hasOption(DOCS_OPTION)) {
      genDocs(
          line.getOptionValue(DOCS_OPTION),
          cwd,
          line.getOptionValue(IN_PROJECT_OPTION),
          logLevel,
          logMasking,
          shouldEnableIrCaches(line, false));
    }
    if (line.hasOption(PREINSTALL_OPTION)) {
      preinstallDependencies(line.getOptionValue(IN_PROJECT_OPTION), logLevel);
    }
    if (line.getOptions().length == 0) {
      printHelp();
      throw exitFail(null);
    }
  }

  /**
   * Checks whether IR caching should be enabled.
   *
   * <p>The (mutually exclusive) flags can control it explicitly, otherwise it defaults to off in
   * development builds and on in production builds.
   *
   * @param line the command-line
   * @return `true` if caching should be enabled, `false`, otherwise
   */
  private boolean shouldEnableIrCaches(CommandLine line, Boolean defaultValue) {
    if (defaultValue == null) {
      defaultValue = !isDevBuild();
    }
    if (line.hasOption(ENABLE_STATIC_ANALYSIS_OPTION)) {
      if (line.hasOption(IR_CACHES_OPTION)) {
        throw exitFail(
            ""
                + ENABLE_STATIC_ANALYSIS_OPTION
                + " requires IR caches to be disabled, so --"
                + IR_CACHES_OPTION
                + " option cannot be used in combination with this flag.");
      }
      return false;
    }
    if (line.hasOption(DISABLE_PRIVATE_CHECK_OPTION)) {
      if (line.hasOption(IR_CACHES_OPTION)) {
        throw exitFail(
            ""
                + DISABLE_PRIVATE_CHECK_OPTION
                + " requires IR caches to be disabled, so --"
                + IR_CACHES_OPTION
                + " option cannot be used in combination with this flag.");
      }
      return false;
    }

    if (line.hasOption(IR_CACHES_OPTION)) {
      return true;
    } else if (line.hasOption(NO_IR_CACHES_OPTION)) {
      return false;
    } else {
      return defaultValue;
    }
  }

  /** Constructs a terminal interface for the REPL, initializing its properties. */
  private static ReplIO makeTerminalForRepl() {
    var env = new Environment() {};
    var distributionManager = new DistributionManager(env);
    var historyFileName = "repl-history.txt";
    var historyFilePath =
        distributionManager.LocallyInstalledDirectories().cacheDirectory().resolve(historyFileName);
    return new TerminalIO(historyFilePath);
  }

  private static <A> A withProfiling(
      ProfilingConfig profilingConfig,
      ExecutionContextExecutor executor,
      java.util.concurrent.Callable<A> main)
      throws IOException {
    var path = profilingConfig.profilingPath();
    var events = profilingConfig.profilingEventsLogPath();
    var pathOS = path.isEmpty() ? null : Files.newOutputStream(path.get());
    var eventsOS = events.isEmpty() ? null : Files.newOutputStream(events.get());
    var sampler = MethodsSampler.create(pathOS, eventsOS);
    sampler.start();
    profilingConfig.profilingTime().foreach(timeout -> sampler.scheduleStop(timeout));
    scala.sys.package$.MODULE$.addShutdownHook(
        () -> {
          try {
            sampler.close();
          } catch (IOException ex) {
            LOGGER.error("Error stopping sampler", ex);
          }
          return BoxedUnit.UNIT;
        });

    try (var _ =
            ObservedMessage.observe(
                LoggerFactory.getLogger("org.enso"),
                (ev) -> {
                  sampler.log(ev.getInstant(), ev.getFormattedMessage());
                });
        var _ =
            ObservedMessage.observe(
                LoggerFactory.getLogger("enso"),
                (ev) -> {
                  sampler.log(ev.getInstant(), ev.getFormattedMessage());
                }); ) {
      return main.call();
    } catch (IOException | RuntimeException ex) {
      throw ex;
    } catch (Exception ex) {
      throw new IOException(ex);
    } finally {
      sampler.close();
    }
  }

  /**
   * Parses all system properties from the given command line.
   *
   * @return null if no cmdline argument was specified.
   */
  protected Map<String, String> parseSystemProperties(CommandLine cmdLine) {
    if (cmdLine.hasOption(SYSTEM_PROPERTY)) {
      Map<String, String> props = new HashMap<>();
      var optionValues = cmdLine.getOptionValues(SYSTEM_PROPERTY);
      for (var optionValue : optionValues) {
        var items = optionValue.split("=");
        if (items.length == 2) {
          props.put(items[0], items[1]);
        } else if (items.length == 1) {
          props.put(items[0], "true");
        } else {
          throw exitFail(
              "Argument to " + SYSTEM_PROPERTY + " must be in the form <property>=<value>");
        }
      }
      return props;
    } else {
      return null;
    }
  }

  private static ProfilingConfig parseProfilingConfig(CommandLine line) throws WrongOption {
    Path profilingPath = null;
    try {
      var path = line.getOptionValue(PROFILING_PATH);
      if (path != null) {
        profilingPath = Paths.get(path);
      }
    } catch (InvalidPathException e) {
      throw new WrongOption("Profiling path is invalid");
    }
    Duration profilingTime = null;
    try {
      var time = line.getOptionValue(PROFILING_TIME);
      if (time != null) {
        profilingTime = Duration.of(Integer.parseInt(time), TimeUnit.SECONDS.toChronoUnit());
      }
    } catch (NumberFormatException e) {
      throw new WrongOption("Profiling time should be an integer");
    }
    return new ProfilingConfig(
        scala.Option.apply(profilingPath), scala.Option.apply(profilingTime));
  }

  private void printPolyglotException(PolyglotException exception, File relativeTo) {
    var msg = HostEnsoUtils.findExceptionMessage(exception);
    Function<StackFrame, String> fnLangId =
        (frame) -> frame.isHostFrame() ? "java" : frame.getLanguage().getId();
    Function<StackFrame, String> fnRootName = StackFrame::getRootName;
    Function<StackFrame, SourceSection> fnSourceSection = StackFrame::getSourceLocation;

    Utils.printStackTrace(
        exception.getPolyglotStackTrace(),
        exception.isSyntaxError(),
        msg,
        relativeTo,
        this::stderr,
        fnLangId,
        fnRootName,
        fnSourceSection);
  }

  @SuppressWarnings("unchecked")
  private static <T> scala.collection.immutable.List<T> nil() {
    return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
  }

  private static final <T> scala.collection.immutable.List<T> join(
      T head, scala.collection.immutable.List<T> tail) {
    return scala.collection.immutable.$colon$colon$.MODULE$.apply(head, tail);
  }

  void stdout(String msg) {
    System.out.println(msg);
  }

  void stderr(String msg) {
    System.err.println(msg);
  }

  /**
   * Checks if JVM mode should be enabled in a project defined by arguments, based on a project's
   * config file, if any.
   *
   * @param cwd current working directory or {@code null}
   * @param line parsed command line arguments
   * @return true, if project should be launched in JVM mode, false otherwise
   */
  private boolean isJvmModeEnabled(String cwd, CommandLine line) {
    try {
      var projectPath = line.getOptionValue(IN_PROJECT_OPTION);
      var path = line.getOptionValue(RUN_OPTION);
      if (path == null) {
        return false;
      }

      var fileAndProject = Utils.findFileAndProject(cwd, path, projectPath);
      if (fileAndProject._3() == null) {
        return false;
      } else {
        var configFile =
            new File(fileAndProject._3())
                .toPath()
                .resolve(org.enso.pkg.Config.ensoPackageConfigName());
        if (!configFile.toFile().exists()) {
          return false;
        } else {
          try (var fileReader = new FileReader(configFile.toFile())) {
            return org.enso.pkg.Config.fromYaml(fileReader)
                .map(c -> c.jvm().getOrElse(() -> false))
                .getOrElse(() -> false);
          }
        }
      }
    } catch (IOException e) {
      return false;
    }
  }

  private void launchJvm(
      String originalCwdOrNull,
      CommandLine line,
      Map<String, String> props,
      File component,
      File javaExecutable)
      throws IOException, InterruptedException {
    /* Cannot use JNI when not in Native Image code. Fallback to launching a process. */
    var useJNI = ImageInfo.inImageCode();
    var commandAndArgs = new ArrayList<String>();
    if (originalCwdOrNull != null) {
      commandAndArgs.add("-Denso.user.dir=" + originalCwdOrNull);
    }
    if (!useJNI) {
      commandAndArgs.add(javaExecutable.getPath());
    }
    var assertsOn = false;
    assert assertsOn = true;
    if (assertsOn) {
      commandAndArgs.add("-ea");
    }
    if (props != null) {
      for (var e : props.entrySet()) {
        commandAndArgs.add("-D" + e.getKey() + "=" + e.getValue());
      }
    }
    commandAndArgs.add("--sun-misc-unsafe-memory-access=allow");
    commandAndArgs.add("--enable-native-access=org.graalvm.truffle");
    commandAndArgs.add("--add-opens=java.base/java.nio=ALL-UNNAMED");
    if (!component.isDirectory()) {
      throw new IOException("Cannot find " + component + " directory");
    }
    JVM jvm;
    if (useJNI) {
      commandAndArgs.add("--module-path=" + component.getPath());
      commandAndArgs.add("-Djdk.module.main=org.enso.runner");
      var javaHome = javaExecutable.getParentFile().getParentFile();
      jvm = JVM.create(javaHome, commandAndArgs.toArray(new String[0]));
      commandAndArgs.clear();
    } else {
      commandAndArgs.add("--module-path");
      commandAndArgs.add(component.getPath());
      commandAndArgs.add("-m");
      commandAndArgs.add("org.enso.runner/org.enso.runner.Main");
      jvm = null;
    }
    var it = line.iterator();
    while (it.hasNext()) {
      var op = it.next();
      if (JVM_OPTION.equals(op.getLongOpt())) {
        continue;
      }
      if (SYSTEM_PROPERTY.equals(op.getLongOpt())) {
        continue;
      }
      var longName = op.getLongOpt();
      if (longName != null) {
        commandAndArgs.add("--" + longName);
      } else {
        commandAndArgs.add("-" + op.getOpt());
      }
      var values = op.getValuesList();
      if (values != null) {
        commandAndArgs.addAll(values);
      }
    }
    commandAndArgs.addAll(line.getArgList());
    int exitCode;
    if (jvm != null) {
      jvm.executeMain("org/enso/runner/Main", commandAndArgs.toArray(new String[0]));
      // the above call should never return
      exitCode = 1;
    } else {
      var pb = new ProcessBuilder();
      pb.inheritIO();
      pb.command(commandAndArgs);
      var p = pb.start();
      exitCode = p.waitFor();
    }
    if (exitCode == 0) {
      throw exitSuccess();
    } else {
      throw doExit(exitCode);
    }
  }

  private void launch(String[] args) throws IOException, InterruptedException, URISyntaxException {
    var line = preprocessArguments(args);

    String originalCwdOrNull = null;
    if (line.hasOption(IN_PROJECT_OPTION)) {
      originalCwdOrNull = Utils.adjustCwdToProject(line.getOptionValue(IN_PROJECT_OPTION));
    } else if (line.hasOption(RUN_OPTION)) {
      originalCwdOrNull = Utils.adjustCwdToProject(line.getOptionValue(RUN_OPTION));
    }

    var logMasking = new boolean[1];
    var props = parseSystemProperties(line);
    if (props != null) {
      for (var e : props.entrySet()) {
        System.setProperty(e.getKey(), e.getValue());
      }
    }
    var logLevel =
        scala.Option.apply(line.getOptionValue(LOG_LEVEL))
            .map(this::parseLogLevel)
            .getOrElse(() -> defaultLogLevel);
    setupLoggingContext(line);
    if (line.hasOption(LANGUAGE_SERVER_OPTION)) {
      // Setup application-ls.conf as the default config file
      // https://github.com/lightbend/config?tab=readme-ov-file#standard-behavior
      // Language Server will also set up logging on its own.
      System.setProperty("config.resource", "application-ls.conf");
    } else {
      setupLogging(line, logLevel, logMasking);
    }

    var loc = Main.class.getProtectionDomain().getCodeSource().getLocation();
    var component = new File(loc.toURI().resolve("..")).getAbsoluteFile();
    if (!component.getName().equals("component")) {
      component = new File(component, "component");
    }
    assert checkOutdatedLauncher(new File(loc.toURI()), component) || true;
    var hasJVMOption = line.hasOption(JVM_OPTION);
    var jvmInProjectEnforced = isJvmModeEnabled(originalCwdOrNull, line);
    if (hasJVMOption || jvmInProjectEnforced) {
      var jvm = line.getOptionValue(JVM_OPTION);
      var current = System.getProperty("java.home");
      if (jvm == null) {
        jvm = current;
      }
      var shouldLaunchJvm = current == null || !current.equals(jvm);
      if (shouldLaunchJvm) {
        var javaExecutable =
            jvm != null
                ? new File(new File(new File(jvm), "bin"), "java").getAbsoluteFile()
                : JavaFinder.findJavaExecutable();
        if (javaExecutable != null) {
          launchJvm(originalCwdOrNull, line, props, component, javaExecutable);
          return;
        } else {
          throw exitFail(
              "Cannot find java executable to run in JVM mode. JVM mode "
                  + "was enforced either by `--jvm` option or by project configuration.");
        }
      }
    }
    if (HostEnsoUtils.isAot()) {
      if (jvmInProjectEnforced) {
        throw exitFail("Cannot find java executable to run in JVM mode");
      } else {
        if (System.getProperty("java.home") == null) {
          var exe = JavaFinder.findJavaExecutable();
          if (exe != null) {
            var path = exe.getParentFile().getParentFile().getAbsolutePath();
            System.setProperty("java.home", path);
            LOGGER.debug("Setting java.home property for AOT mode to {}", path);
          }
        }
      }
    }
    handleLaunch(originalCwdOrNull, line, logLevel, logMasking[0]);
  }

  final CommandLine preprocessArguments(String... args) {
    var parser = new DefaultParser();
    try {
      var startParsing = System.currentTimeMillis();
      var line = parser.parse(CLI_OPTIONS, args);
      LOGGER.trace(
          "Parsing Language Server arguments took {0}ms",
          System.currentTimeMillis() - startParsing);
      return line;
    } catch (Exception e) {
      printHelp();
      throw exitFail(e.getMessage());
    }
  }

  private void setupLoggingContext(CommandLine line) {
    String projectId;
    var projectIdOptional = line.getOptionValue(LanguageServerApi.PROJECT_ID_OPTION);
    try {
      // sanity check
      projectId =
          projectIdOptional != null
              ? UUID.fromString(projectIdOptional).toString()
              : "00000000-0000-0000-0000-000000000000";
    } catch (IllegalArgumentException e) {
      projectId = "00000000-0000-0000-0000-000000000000";
    }
    if (line.hasOption(LanguageServerApi.CLOUD_PROJECT_ID_OPTION)) {
      MDC.put("projectId", line.getOptionValue(LanguageServerApi.CLOUD_PROJECT_ID_OPTION));
    } else if (System.getenv(LanguageServerApi.ENSO_CLOUD_PROJECT_ID_ENV_NAME) != null) {
      MDC.put("projectId", System.getenv(LanguageServerApi.ENSO_CLOUD_PROJECT_ID_ENV_NAME));
    }
    if (line.hasOption(LanguageServerApi.CLOUD_PROJECT_SESSION_ID_OPTION)) {
      MDC.put(
          "projectSessionId",
          line.getOptionValue(LanguageServerApi.CLOUD_PROJECT_SESSION_ID_OPTION));
    } else if (System.getenv(LanguageServerApi.ENSO_CLOUD_PROJECT_SESSION_ID_ENV_NAME) != null) {
      MDC.put(
          "projectSessionId",
          System.getenv(LanguageServerApi.ENSO_CLOUD_PROJECT_SESSION_ID_ENV_NAME));
    }
    MDC.put("projectLocalId", projectId);
  }

  private Level setupLogging(CommandLine line, Level logLevel, boolean[] logMasking) {
    URI connectionUri;
    if (line.getOptionValue(LOGGER_CONNECT) != null) {
      connectionUri = parseUri(line.getOptionValue(LOGGER_CONNECT));
    } else {
      connectionUri = null;
    }
    logMasking[0] = !line.hasOption(NO_LOG_MASKING);
    RunnerLogging.setup(connectionUri, logLevel, logMasking[0]);
    return logLevel;
  }

  private final void handleLaunch(
      String cwd, CommandLine line, Level logLevel, boolean logMasking) {
    if (line.hasOption(LANGUAGE_SERVER_OPTION)) {
      try {
        var conf = parseProfilingConfig(line);
        LanguageServerApi.launchLanguageServer(line, conf, logLevel);
        throw exitSuccess();
      } catch (WrongOption e) {
        throw exitFail(e.getMessage());
      }
    } else {
      if (line.hasOption(LANGUAGE_SERVER_NATIVE_OPTION)) {
        stderr(
            "\"--"
                + LANGUAGE_SERVER_NATIVE_OPTION
                + "\" has no effect without --\""
                + LANGUAGE_SERVER_OPTION
                + "\"");
      }
      try {
        var conf = parseProfilingConfig(line);
        try {
          withProfiling(
              conf,
              ExecutionContext.global(),
              () -> {
                mainEntry(cwd, line, logLevel, logMasking);
                return BoxedUnit.UNIT;
              });
        } catch (ExitCode ex) {
          throw exitFail(ex.getMessage());
        } catch (IOException ex) {
          if (LOGGER.isDebugEnabled()) {
            LOGGER.error("Error during execution", ex);
          }
          throw exitFail("Command failed with an error: " + ex.getMessage());
        }
      } catch (WrongOption e) {
        throw exitFail(e.getMessage());
      }
    }
  }

  /**
   * Check if native image based launcher is up-to-date. Prints a warning when it is outdated.
   *
   * @param base the base file to check
   * @param dir directory with other files that should be older than base
   * @return
   */
  private boolean checkOutdatedLauncher(File base, File dir) {
    var needsCheck = base.canExecute();
    if (needsCheck) {
      var files = dir.listFiles();
      if (files != null) {
        var baseTime = base.lastModified();
        for (var f : files) {
          if (baseTime < f.lastModified()) {
            stderr("File " + base + " is older than " + f + " consider running in --jvm mode");
            return false;
          }
        }
      }
    }
    return true;
  }
}
