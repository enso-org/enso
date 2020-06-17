package org.enso.polyglot;

import java.util.logging.Level;
import org.graalvm.options.OptionDescriptor;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;

import java.util.Arrays;

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

  public static final String LOG_LEVEL = "log.level";
  public static final OptionKey<String> LOG_LEVEL_KEY = new OptionKey<>(Level.INFO.toString());
  private static final OptionDescriptor LOG_LEVEL_DESCRIPTOR =
      OptionDescriptor.newBuilder(LOG_LEVEL_KEY, LOG_LEVEL).build();

  public static final OptionDescriptors OPTION_DESCRIPTORS =
      OptionDescriptors.create(
          Arrays.asList(PACKAGES_PATH_DESCRIPTOR, STRICT_ERRORS_DESCRIPTOR,
                  LOG_LEVEL_DESCRIPTOR));

  /**
   * Canonicalizes the option name by prefixing it with the language name.
   *
   * @param name the simplified option name
   * @return the canonicalized representation of the option.
   */
  private static String optionName(String name) {
    return LanguageInfo.ID + "." + name;
  }
}
