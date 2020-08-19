package org.enso.polyglot;

import org.graalvm.options.OptionDescriptor;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;

import java.util.Arrays;
import java.util.logging.Level;

/** Class representing runtime options supported by the Enso engine. */
public class RuntimeOptions {
  public static final String PACKAGES_PATH = optionName("packagesPath");
  public static final OptionKey<String> PACKAGES_PATH_KEY = new OptionKey<>("");
  private static final OptionDescriptor PACKAGES_PATH_DESCRIPTOR =
      OptionDescriptor.newBuilder(PACKAGES_PATH_KEY, PACKAGES_PATH).build();

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

  public static final String INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION =
      interpreterOptionName(".sequentialCommandExecution");
  public static final OptionKey<Boolean> INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_KEY =
      new OptionKey<>(false);
  public static final OptionDescriptor INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_DESCRIPTOR =
      OptionDescriptor.newBuilder(
              INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_KEY,
              INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION)
          .build();

  public static final OptionDescriptors OPTION_DESCRIPTORS =
      OptionDescriptors.create(
          Arrays.asList(
              PACKAGES_PATH_DESCRIPTOR,
              STRICT_ERRORS_DESCRIPTOR,
              LOG_LEVEL_DESCRIPTOR,
              DISABLE_INLINE_CACHES_DESCRIPTOR,
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
