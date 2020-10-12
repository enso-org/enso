package org.enso.interpreter;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.polyglot.RuntimeOptions;

public class OptionsHelper {
  /**
   * Gets the list of locations of known modules that can be imported in the current run.
   *
   * @param env the current run environment
   * @return the list of locations of known modules that can be imported in the current run
   */
  public static List<TruffleFile> getPackagesPaths(TruffleLanguage.Env env) {
    if (env.getOptions().get(RuntimeOptions.PACKAGES_PATH_KEY).equals("")) {
      return Collections.emptyList();
    } else {
      return Arrays.stream(
              env.getOptions().get(RuntimeOptions.PACKAGES_PATH_KEY).split(env.getPathSeparator()))
          .map(env::getInternalTruffleFile)
          .collect(Collectors.toList());
    }
  }
}
