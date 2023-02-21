package org.enso.polyglot;

import java.util.Arrays;

import org.graalvm.options.*;
import com.oracle.truffle.api.Option;

/** Class representing runtime options supported by the Enso engine. */
public class RuntimeOptions {
  public static final String PROJECT_ROOT = optionName("projectRoot");
  public static final OptionKey<String> PROJECT_ROOT_KEY = new OptionKey<>("");
  private static final OptionDescriptor PROJECT_ROOT_DESCRIPTOR =
      OptionDescriptor.newBuilder(PROJECT_ROOT_KEY, PROJECT_ROOT).build();

  public static final String STRICT_ERRORS = optionName("strictErrors");
  public static final OptionKey<Boolean> STRICT_ERRORS_KEY = new OptionKey<>(false);
  private static final OptionDescriptor STRICT_ERRORS_DESCRIPTOR =
      OptionDescriptor.newBuilder(STRICT_ERRORS_KEY, STRICT_ERRORS).build();

  public static final String DISABLE_INLINE_CACHES = optionName("disableInlineCaches");
  public static final OptionKey<Boolean> DISABLE_INLINE_CACHES_KEY = new OptionKey<>(false);
  private static final OptionDescriptor DISABLE_INLINE_CACHES_DESCRIPTOR =
      OptionDescriptor.newBuilder(DISABLE_INLINE_CACHES_KEY, DISABLE_INLINE_CACHES).build();

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

  public static final String INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION =
      interpreterOptionName("sequentialCommandExecution");
  public static final OptionKey<Boolean> INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_KEY =
      new OptionKey<>(false);
  public static final OptionDescriptor INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_DESCRIPTOR =
      OptionDescriptor.newBuilder(
              INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_KEY,
              INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION)
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

  public static final String USE_GLOBAL_IR_CACHE_LOCATION = optionName("useGlobalIrCacheLocation");
  public static final OptionKey<Boolean> USE_GLOBAL_IR_CACHE_LOCATION_KEY = new OptionKey<>(true);
  public static final OptionDescriptor USE_GLOBAL_IR_CACHE_LOCATION_DESCRIPTOR =
      OptionDescriptor.newBuilder(USE_GLOBAL_IR_CACHE_LOCATION_KEY, USE_GLOBAL_IR_CACHE_LOCATION)
          .build();

  public static final String ENABLE_EXECUTION_TIMER = optionName("enableExecutionTimer");

  @Option(
      help = "Enables timer that counts down the execution time of expressions.",
      category = OptionCategory.INTERNAL)
  public static final OptionKey<Boolean> ENABLE_EXECUTION_TIMER_KEY = new OptionKey<>(true);

  private static final OptionDescriptor ENABLE_EXECUTION_TIMER_DESCRIPTOR =
      OptionDescriptor.newBuilder(ENABLE_EXECUTION_TIMER_KEY, ENABLE_EXECUTION_TIMER).build();

  public static final OptionDescriptors OPTION_DESCRIPTORS =
      OptionDescriptors.create(
          Arrays.asList(
              PROJECT_ROOT_DESCRIPTOR,
              STRICT_ERRORS_DESCRIPTOR,
              LOG_MASKING_DESCRIPTOR,
              DISABLE_INLINE_CACHES_DESCRIPTOR,
              ENABLE_AUTO_PARALLELISM_DESCRIPTOR,
              ENABLE_PROJECT_SUGGESTIONS_DESCRIPTOR,
              ENABLE_GLOBAL_SUGGESTIONS_DESCRIPTOR,
              INTERACTIVE_MODE_DESCRIPTOR,
              LANGUAGE_HOME_OVERRIDE_DESCRIPTOR,
              EDITION_OVERRIDE_DESCRIPTOR,
              INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_DESCRIPTOR,
              DISABLE_IR_CACHES_DESCRIPTOR,
              PREINITIALIZE_DESCRIPTOR,
              WAIT_FOR_PENDING_SERIALIZATION_JOBS_DESCRIPTOR,
              USE_GLOBAL_IR_CACHE_LOCATION_DESCRIPTOR,
              ENABLE_EXECUTION_TIMER_DESCRIPTOR));

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
