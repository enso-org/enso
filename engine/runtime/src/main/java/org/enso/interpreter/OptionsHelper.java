package org.enso.interpreter;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import java.io.File;
import java.net.URISyntaxException;
import java.util.Optional;
import org.enso.common.RuntimeOptions;

public final class OptionsHelper {
  private OptionsHelper() {}

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
   * Finds location of language home directory. It checks {@link
   * RuntimeOptions#LANGUAGE_HOME_OVERRIDE} and uses it. If it is not specified, it derives the
   * location from code source location of the JAR file.
   *
   * <p>This is used mostly for the runtime tests, as language home is not normally defined there.
   */
  public static Optional<String> findLanguageHome(TruffleLanguage.Env env) {
    String option = env.getOptions().get(RuntimeOptions.LANGUAGE_HOME_OVERRIDE_KEY);
    if (!option.equals("")) {
      return Optional.of(option);
    }
    try {
      var cs = OptionsHelper.class.getProtectionDomain().getCodeSource();
      var runtimeJarUri = cs.getLocation().toURI();
      var runtimeJar = new File(runtimeJarUri);
      var componentDir = runtimeJar.getParentFile();
      if (componentDir != null && componentDir.isDirectory()) {
        return Optional.of(componentDir.getPath());
      }
    } catch (IllegalStateException | URISyntaxException ex) {
      // cannot derive the location
    }
    return Optional.empty();
  }

  public static Optional<String> getEditionOverride(TruffleLanguage.Env env) {
    String option = env.getOptions().get(RuntimeOptions.EDITION_OVERRIDE_KEY);
    if (option.equals("")) {
      return Optional.empty();
    } else {
      return Optional.of(option);
    }
  }
}
