package org.enso.interpreter;

import com.oracle.truffle.api.TruffleLanguage;
import org.enso.polyglot.RuntimeOptions;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class OptionsHelper {
  /**
   * Gets the list of locations of known modules that can be imported in the current run.
   *
   * @param env the current run environment
   * @return the list of locations of known modules that can be imported in the current run
   */
  public static List<File> getPackagesPaths(TruffleLanguage.Env env) {
    return Arrays.stream(env.getOptions().get(RuntimeOptions.PACKAGES_PATH_KEY).split(env.getPathSeparator()))
        .map(File::new)
        .collect(Collectors.toList());
  }
}
