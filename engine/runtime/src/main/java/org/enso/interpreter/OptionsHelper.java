package org.enso.interpreter;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.enso.polyglot.RuntimeOptions;

public class OptionsHelper {
  /**
   * Gets the location of the project that is the context of the current run.
   *
   * @param env the current run environment
   * @return the project path (can be empty if running outside of the project)
   */
  public static Optional<TruffleFile> getProjectRoot(TruffleLanguage.Env env) {
    String option = env.getOptions().get(RuntimeOptions.PROJECT_ROOT_KEY);
    if (option.equals("")) {
      return Optional.empty();
    } else {
      return Optional.of(env.getInternalTruffleFile(option));
    }
  }

  /**
   * Gets the list of locations of packages that should be preloaded.
   *
   * <p>This is meant mainly for testing purposes, as normally package locations are handled by
   * environment variables, see {@link org.enso.distribution.DistributionManager}.
   *
   * <p>It will also be slightly repurposed after integrating with editions and once the standard
   * library directory structure is upgraded to the new format.
   */
  public static List<TruffleFile> getPreloadedPackagesPaths(TruffleLanguage.Env env) {
    String option = env.getOptions().get(RuntimeOptions.PRELOADED_PACKAGES_PATHS_KEY);
    if (option.equals("")) {
      return Collections.emptyList();
    } else {
      return Arrays.stream(
          option.split(env.getPathSeparator()))
          .map(env::getInternalTruffleFile)
          .collect(Collectors.toList());
    }
  }
}
