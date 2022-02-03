package org.enso.interpreter;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import java.util.Optional;
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
   * Gets an optional override for the language home directory.
   *
   * This is used mostly for the runtime tests, as language home is not normally
   * defined there.
   */
  public static Optional<String> getLanguageHomeOverride(TruffleLanguage.Env env) {
    String option = env.getOptions().get(RuntimeOptions.LANGUAGE_HOME_OVERRIDE_KEY);
    if (option.equals("")) {
      return Optional.empty();
    } else {
      return Optional.of(option);
    }
  }
}
