package org.enso.runner;

import java.io.File;
import java.io.IOException;
import scala.Tuple3;

final class Utils {
  private Utils() {}

  /**
   * Verifies path and project path.
   *
   * @param path file or project to execute
   * @param projectPath project path or {@code null} if it hasn't been specified
   * @return tuple with boolean, File to execute and path for project to use or {@code null} if
   *     execution shall finish
   */
  static scala.Tuple3<Boolean, File, String> findFileAndProject(String path, String projectPath)
      throws IOException {
    var file = new File(path);
    if (!file.exists()) {
      System.err.println("File " + file + " does not exist.");
      return null;
    }
    var projectMode = file.isDirectory();
    var canonicalFile = file.getCanonicalFile();
    String projectRoot;
    if (projectMode) {
      if (projectPath != null) {
        var canonicalProjectFile = new File(projectPath).getCanonicalFile();
        if (!canonicalProjectFile.equals(canonicalFile)) {
          var msg =
              "It is not possible to run a project ("
                  + canonicalFile
                  + ") in context of another "
                  + "project ("
                  + canonicalProjectFile
                  + "), please do not use the `--in-project` option for "
                  + "running projects.";
          System.err.println(msg);
          return null;
        }
      }
      projectRoot = canonicalFile.getPath();
    } else {
      if (projectPath != null) {
        projectRoot = projectPath;
      } else {
        var f = canonicalFile;
        for (; ; ) {
          if (f == null) {
            projectRoot = "";
            break;
          } else {
            var p = f.getParentFile();
            if ("src".equals(f.getName())) {
              if (p != null && new File(p, "package.yaml").isFile()) {
                projectRoot = p.getPath();
                break;
              }
            }
            f = p;
          }
        }
      }
    }
    return Tuple3.apply(projectMode, canonicalFile, projectRoot);
  }
}
