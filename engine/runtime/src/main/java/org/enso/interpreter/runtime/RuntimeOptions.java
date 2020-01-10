package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleLanguage;
import org.enso.interpreter.Constants;
import org.enso.polyglot.LanguageInfo;
import org.graalvm.options.OptionDescriptor;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/** Class representing runtime options supported by the Enso engine. */
public class RuntimeOptions {
  private static final String PACKAGES_PATH = optionName("packagesPath");
  private static final OptionKey<String> PACKAGES_PATH_KEY = new OptionKey<>("");
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
   * Gets the list of locations of known modules that can be imported in the current run.
   *
   * @param env the current run environment
   * @return the list of locations of known modules that can be imported in the current run
   */
  public static List<File> getPackagesPaths(TruffleLanguage.Env env) {
    return Arrays.stream(env.getOptions().get(PACKAGES_PATH_KEY).split(env.getPathSeparator()))
        .map(File::new)
        .collect(Collectors.toList());
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
