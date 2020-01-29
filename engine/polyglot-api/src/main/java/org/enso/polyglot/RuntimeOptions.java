package org.enso.polyglot;

import org.enso.polyglot.LanguageInfo;
import org.graalvm.options.OptionDescriptor;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;

import java.util.Collections;

/** Class representing runtime options supported by the Enso engine. */
public class RuntimeOptions {
  private static final String PACKAGES_PATH = optionName("packagesPath");
  public static final OptionKey<String> PACKAGES_PATH_KEY = new OptionKey<>("");
  private static final OptionDescriptor PACKAGES_PATH_DESCRIPTOR =
      OptionDescriptor.newBuilder(PACKAGES_PATH_KEY, getPackagesPathOption()).build();
  public static final OptionDescriptors OPTION_DESCRIPTORS =
      OptionDescriptors.create(Collections.singletonList(PACKAGES_PATH_DESCRIPTOR));

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
   * An option to pass a $PATH-like list of locations of all known modules that can be imported in
   * the current run.
   *
   * @return the name of this option
   */
  public static String getPackagesPathOption() {
    return PACKAGES_PATH;
  }
}
