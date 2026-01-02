package org.enso.common;

import java.util.Arrays;
import org.graalvm.options.OptionCategory;
import org.graalvm.options.OptionDescriptor;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;

/** Class representing runtime options supported by the Enso engine. */
public final class RuntimeOptions {
  private RuntimeOptions() {}

  public static final String PROJECT_ROOT = optionName("projectRoot");
  public static final OptionKey<String> PROJECT_ROOT_KEY = new OptionKey<>("");
  private static final OptionDescriptor PROJECT_ROOT_DESCRIPTOR =
      OptionDescriptor.newBuilder(PROJECT_ROOT_KEY, PROJECT_ROOT).build();

  public static final String STRICT_ERRORS = optionName("strictErrors");
  public static final OptionKey<Boolean> STRICT_ERRORS_KEY = new OptionKey<>(true);
  private static final OptionDescriptor STRICT_ERRORS_DESCRIPTOR =
      OptionDescriptor.newBuilder(STRICT_ERRORS_KEY, STRICT_ERRORS).build();

  public static final String DISABLE_INLINE_CACHES = optionName("disableInlineCaches");
  public static final OptionKey<Boolean> DISABLE_INLINE_CACHES_KEY = new OptionKey<>(false);
  private static final OptionDescriptor DISABLE_INLINE_CACHES_DESCRIPTOR =
      OptionDescriptor.newBuilder(DISABLE_INLINE_CACHES_KEY, DISABLE_INLINE_CACHES).build();

  public static final String DISABLE_PRIVATE_CHECK = optionName("disablePrivateCheck");
  public static final OptionKey<Boolean> DISABLE_PRIVATE_CHECK_KEY = new OptionKey<>(false);
  private static final OptionDescriptor DISABLE_PRIVATE_CHECK_DESCRIPTOR =
      OptionDescriptor.newBuilder(DISABLE_PRIVATE_CHECK_KEY, DISABLE_PRIVATE_CHECK).build();

  public static final String ENABLE_STATIC_ANALYSIS = optionName("enableStaticAnalysis");
  public static final OptionKey<Boolean> ENABLE_STATIC_ANALYSIS_KEY = new OptionKey<>(false);
  private static final OptionDescriptor ENABLE_STATIC_ANALYSIS_DESCRIPTOR =
      OptionDescriptor.newBuilder(ENABLE_STATIC_ANALYSIS_KEY, ENABLE_STATIC_ANALYSIS).build();

  public static final String CHECK_CWD = optionName("checkCurrentDirectory");
  public static final OptionKey<Boolean> CHECK_CWD_KEY = new OptionKey<>(true);
  private static final OptionDescriptor CHECK_CWD_DESCRIPTOR =
      OptionDescriptor.newBuilder(CHECK_CWD_KEY, CHECK_CWD)
          .help("Should Enso check for current working directory")
          .category(OptionCategory.INTERNAL)
          .build();

  public static final String EDITIONS_DIRECTORY = optionName("editionsDirectory");
  public static final OptionKey<String> EDITIONS_DIRECTORY_KEY = new OptionKey<>("");
  private static final OptionDescriptor EDITIONS_DIRECTORY_DESCRIPTOR =
      OptionDescriptor.newBuilder(EDITIONS_DIRECTORY_KEY, EDITIONS_DIRECTORY)
          .help("Directory with custom editions")
          .category(OptionCategory.INTERNAL)
          .build();

  public static final String HOST_CLASS_LOADING = optionName("classLoading");
  public static final String HOST_CLASS_LOADING_HOSTED = "hosted";
  public static final String HOST_CLASS_LOADING_GUEST = "guest";
  public static final OptionKey<String> HOST_CLASS_LOADING_KEY =
      new OptionKey<>(HOST_CLASS_LOADING_HOSTED);
  private static final OptionDescriptor HOST_CLASS_LOADING_DESCRIPTOR =
      OptionDescriptor.newBuilder(HOST_CLASS_LOADING_KEY, HOST_CLASS_LOADING)
          .help("Controls the way Enso runtime resolves polyglot java import statements")
          .usageSyntax("Expecting comma separated list of `[<namespace>.<name>:]?hosted|guest`")
          .category(OptionCategory.INTERNAL)
          .build();

  public static final String TREAT_WARNINGS_AS_ERRORS = optionName("treatWarningsAsErrors");
  public static final OptionKey<Boolean> TREAT_WARNINGS_AS_ERRORS_KEY = new OptionKey<>(false);
  private static final OptionDescriptor TREAT_WARNINGS_AS_ERRORS_DESCRIPTOR =
      OptionDescriptor.newBuilder(TREAT_WARNINGS_AS_ERRORS_KEY, TREAT_WARNINGS_AS_ERRORS).build();

  public static final String ENABLE_AUTO_PARALLELISM = optionName("withAutoParallelism");
  public static final OptionKey<Boolean> ENABLE_AUTO_PARALLELISM_KEY = new OptionKey<>(false);
  private static final OptionDescriptor ENABLE_AUTO_PARALLELISM_DESCRIPTOR =
      OptionDescriptor.newBuilder(ENABLE_AUTO_PARALLELISM_KEY, ENABLE_AUTO_PARALLELISM).build();

  public static final String LOG_LEVEL = "log.level";

  public static final String LOG_MASKING = optionName("log.masking");
  public static final OptionKey<Boolean> LOG_MASKING_KEY = new OptionKey<>(true);
  private static final OptionDescriptor LOG_MASKING_DESCRIPTOR =
      OptionDescriptor.newBuilder(LOG_MASKING_KEY, LOG_MASKING).build();

  public static final String INTERACTIVE_MODE = interpreterOptionName("interactive");
  public static final OptionKey<Boolean> INTERACTIVE_MODE_KEY = new OptionKey<>(false);
  public static final OptionDescriptor INTERACTIVE_MODE_DESCRIPTOR =
      OptionDescriptor.newBuilder(INTERACTIVE_MODE_KEY, INTERACTIVE_MODE).build();

  public static final String DISABLE_LINTING = optionName("disableLinting");
  public static final OptionKey<Boolean> DISABLE_LINTING_KEY = new OptionKey<>(false);
  public static final OptionDescriptor DISABLE_LINTING_DESCRIPTOR =
      OptionDescriptor.newBuilder(DISABLE_LINTING_KEY, DISABLE_LINTING).build();

  public static final String INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION =
      interpreterOptionName("sequentialCommandExecution");
  public static final OptionKey<Boolean> INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_KEY =
      new OptionKey<>(false);
  public static final OptionDescriptor INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_DESCRIPTOR =
      OptionDescriptor.newBuilder(
              INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_KEY,
              INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION)
          .build();

  public static final String INTERPRETER_RANDOM_DELAYED_COMMAND_EXECUTION =
      interpreterOptionName("randomDelayedCommandExecution");
  public static final OptionKey<Boolean> INTERPRETER_RANDOM_DELAYED_COMMAND_EXECUTION_KEY =
      new OptionKey<>(false);
  public static final OptionDescriptor INTERPRETER_RANDOM_DELAYED_COMMAND_EXECUTION_DESCRIPTOR =
      OptionDescriptor.newBuilder(
              INTERPRETER_RANDOM_DELAYED_COMMAND_EXECUTION_KEY,
              INTERPRETER_RANDOM_DELAYED_COMMAND_EXECUTION)
          .build();

  public static final String JOB_PARALLELISM = interpreterOptionName("jobParallelism");
  public static final OptionKey<Integer> JOB_PARALLELISM_KEY = new OptionKey<>(1);
  public static final OptionDescriptor JOB_PARALLELISM_DESCRIPTOR =
      OptionDescriptor.newBuilder(JOB_PARALLELISM_KEY, JOB_PARALLELISM).build();

  public static final String GUEST_PARALLELISM = interpreterOptionName("guestParallelism");
  public static final OptionKey<Integer> GUEST_PARALLELISM_KEY = new OptionKey<>(1);
  public static final OptionDescriptor GUEST_PARALLELISM_DESCRIPTOR =
      OptionDescriptor.newBuilder(GUEST_PARALLELISM_KEY, GUEST_PARALLELISM)
          .category(OptionCategory.EXPERT)
          .build();

  public static final String ENABLE_PROJECT_SUGGESTIONS = optionName("enableProjectSuggestions");
  public static final OptionKey<Boolean> ENABLE_PROJECT_SUGGESTIONS_KEY = new OptionKey<>(true);
  private static final OptionDescriptor ENABLE_PROJECT_SUGGESTIONS_DESCRIPTOR =
      OptionDescriptor.newBuilder(ENABLE_PROJECT_SUGGESTIONS_KEY, ENABLE_PROJECT_SUGGESTIONS)
          .build();

  public static final String ENABLE_GLOBAL_SUGGESTIONS = optionName("enableGlobalSuggestions");
  public static final OptionKey<Boolean> ENABLE_GLOBAL_SUGGESTIONS_KEY = new OptionKey<>(true);
  private static final OptionDescriptor ENABLE_GLOBAL_SUGGESTIONS_DESCRIPTOR =
      OptionDescriptor.newBuilder(ENABLE_GLOBAL_SUGGESTIONS_KEY, ENABLE_GLOBAL_SUGGESTIONS).build();

  public static final String ENABLE_PROGRESS_REPORT = optionName("enableProgressReport");
  public static final OptionKey<Boolean> ENABLE_PROGRESS_REPORT_KEY = new OptionKey<>(true);
  private static final OptionDescriptor ENABLE_PROGRESS_REPORT_DESCRIPTOR =
      OptionDescriptor.newBuilder(ENABLE_PROGRESS_REPORT_KEY, ENABLE_PROGRESS_REPORT).build();

  public static final String LANGUAGE_HOME_OVERRIDE = optionName("languageHomeOverride");
  public static final OptionKey<String> LANGUAGE_HOME_OVERRIDE_KEY = new OptionKey<>("");
  private static final OptionDescriptor LANGUAGE_HOME_OVERRIDE_DESCRIPTOR =
      OptionDescriptor.newBuilder(LANGUAGE_HOME_OVERRIDE_KEY, LANGUAGE_HOME_OVERRIDE).build();

  public static final String EDITION_OVERRIDE = optionName("editionOverride");
  public static final OptionKey<String> EDITION_OVERRIDE_KEY = new OptionKey<>("");
  private static final OptionDescriptor EDITION_OVERRIDE_DESCRIPTOR =
      OptionDescriptor.newBuilder(EDITION_OVERRIDE_KEY, EDITION_OVERRIDE).build();

  public static final String DISABLE_IR_CACHES = optionName("disableIrCaches");
  public static final String PREINITIALIZE = optionName("preinitialize");
  public static final OptionKey<String> PREINITIALIZE_KEY = new OptionKey<>("");
  private static final OptionDescriptor PREINITIALIZE_DESCRIPTOR =
      OptionDescriptor.newBuilder(PREINITIALIZE_KEY, PREINITIALIZE).build();
  public static final OptionKey<Boolean> DISABLE_IR_CACHES_KEY = new OptionKey<>(false);
  private static final OptionDescriptor DISABLE_IR_CACHES_DESCRIPTOR =
      OptionDescriptor.newBuilder(DISABLE_IR_CACHES_KEY, DISABLE_IR_CACHES).build();

  public static final String WAIT_FOR_PENDING_SERIALIZATION_JOBS =
      optionName("waitForPendingSerializationJobs");
  public static final OptionKey<Boolean> WAIT_FOR_PENDING_SERIALIZATION_JOBS_KEY =
      new OptionKey<>(false);
  private static final OptionDescriptor WAIT_FOR_PENDING_SERIALIZATION_JOBS_DESCRIPTOR =
      OptionDescriptor.newBuilder(
              WAIT_FOR_PENDING_SERIALIZATION_JOBS_KEY, WAIT_FOR_PENDING_SERIALIZATION_JOBS)
          .build();

  public static final String ENABLE_EXECUTION_TIMER = optionName("enableExecutionTimer");

  /* Enables timer that counts down the execution time of expressions. */
  public static final OptionKey<Boolean> ENABLE_EXECUTION_TIMER_KEY = new OptionKey<>(true);

  private static final OptionDescriptor ENABLE_EXECUTION_TIMER_DESCRIPTOR =
      OptionDescriptor.newBuilder(ENABLE_EXECUTION_TIMER_KEY, ENABLE_EXECUTION_TIMER).build();

  public static final String WARNINGS_LIMIT = optionName("warningsLimit");

  /* Maximal number of warnings that can be attached to a value. */
  public static final OptionKey<Integer> WARNINGS_LIMIT_KEY = new OptionKey<>(100);

  private static final OptionDescriptor WARNINGS_LIMIT_DESCRIPTOR =
      OptionDescriptor.newBuilder(WARNINGS_LIMIT_KEY, WARNINGS_LIMIT).build();

  public static final String IR_DUMPER_SYSTEM_PROP = "enso.compiler.dumpIr";
  public static final String REMOVE_UNUSED_IMPORTS_SYSTEM_PROP =
      "enso.compiler.removeUnusedImports";

  public static final OptionDescriptors OPTION_DESCRIPTORS =
      OptionDescriptors.create(
          Arrays.asList(
              PROJECT_ROOT_DESCRIPTOR,
              STRICT_ERRORS_DESCRIPTOR,
              LOG_MASKING_DESCRIPTOR,
              DISABLE_INLINE_CACHES_DESCRIPTOR,
              DISABLE_PRIVATE_CHECK_DESCRIPTOR,
              ENABLE_STATIC_ANALYSIS_DESCRIPTOR,
              HOST_CLASS_LOADING_DESCRIPTOR,
              CHECK_CWD_DESCRIPTOR,
              EDITIONS_DIRECTORY_DESCRIPTOR,
              TREAT_WARNINGS_AS_ERRORS_DESCRIPTOR,
              ENABLE_AUTO_PARALLELISM_DESCRIPTOR,
              ENABLE_PROJECT_SUGGESTIONS_DESCRIPTOR,
              ENABLE_GLOBAL_SUGGESTIONS_DESCRIPTOR,
              ENABLE_PROGRESS_REPORT_DESCRIPTOR,
              INTERACTIVE_MODE_DESCRIPTOR,
              DISABLE_LINTING_DESCRIPTOR,
              LANGUAGE_HOME_OVERRIDE_DESCRIPTOR,
              EDITION_OVERRIDE_DESCRIPTOR,
              INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_DESCRIPTOR,
              INTERPRETER_RANDOM_DELAYED_COMMAND_EXECUTION_DESCRIPTOR,
              JOB_PARALLELISM_DESCRIPTOR,
              GUEST_PARALLELISM_DESCRIPTOR,
              DISABLE_IR_CACHES_DESCRIPTOR,
              PREINITIALIZE_DESCRIPTOR,
              WAIT_FOR_PENDING_SERIALIZATION_JOBS_DESCRIPTOR,
              ENABLE_EXECUTION_TIMER_DESCRIPTOR,
              WARNINGS_LIMIT_DESCRIPTOR));

  /**
   * Canonicalizes the option name by prefixing it with the language name.
   *
   * @param name the simplified option name
   * @return the canonicalized representation of the option.
   */
  private static String optionName(String name) {
    return LanguageInfo.ID + "." + name;
  }

  /**
   * Canonicalizes the option name by prefixing it with the 'interpreter' subname.
   *
   * @param name the simplified option name
   * @return the canonicalized representation of the option.
   */
  private static String interpreterOptionName(String name) {
    return LanguageInfo.ID + ".interpreter." + name;
  }
}
