package org.enso.polyglot;

import java.util.Arrays;
import java.util.logging.Level;
import org.graalvm.options.OptionDescriptor;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;

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

  public static final String LOG_LEVEL = "log.level";
  public static final OptionKey<String> LOG_LEVEL_KEY = new OptionKey<>(Level.INFO.toString());
  private static final OptionDescriptor LOG_LEVEL_DESCRIPTOR =
      OptionDescriptor.newBuilder(LOG_LEVEL_KEY, LOG_LEVEL).build();

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

  public static final OptionDescriptors OPTION_DESCRIPTORS =
      OptionDescriptors.create(
          Arrays.asList(
              PROJECT_ROOT_DESCRIPTOR,
              STRICT_ERRORS_DESCRIPTOR,
              LOG_LEVEL_DESCRIPTOR,
              DISABLE_INLINE_CACHES_DESCRIPTOR,
              ENABLE_PROJECT_SUGGESTIONS_DESCRIPTOR,
              ENABLE_GLOBAL_SUGGESTIONS_DESCRIPTOR,
              INTERACTIVE_MODE_DESCRIPTOR,
              LANGUAGE_HOME_OVERRIDE_DESCRIPTOR,
              INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_DESCRIPTOR));

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
