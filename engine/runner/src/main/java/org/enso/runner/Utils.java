package org.enso.runner;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.function.Consumer;
import java.util.function.Function;
import org.enso.common.LanguageInfo;
import org.enso.os.environment.chdir.WorkingDirectory;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.polyglot.SourceSection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Tuple3;

final class Utils {
  private static final Logger LOGGER = LoggerFactory.getLogger(Utils.class);

  private Utils() {}

  /** Verifies a file exists. */
  private static scala.Tuple2<Boolean, File> fileExists(String cwd, String path) {
    if (cwd != null) {
      var relativeFile = new File(cwd, path);
      var relativeFileExists = relativeFile.exists();
      LOGGER.debug(
          "Checking cwd {} and file {} - e.g. {} if it exists {}",
          cwd,
          path,
          relativeFile.getAbsolutePath(),
          relativeFileExists);
      if (relativeFileExists) {
        return scala.Tuple2.apply(relativeFileExists, relativeFile);
      }
    }
    var file = new File(path);
    var fileExists = file.exists();
    LOGGER.debug(
        "Checking file {} - e.g. {} if it exists {}", path, file.getAbsolutePath(), fileExists);
    return scala.Tuple2.apply(fileExists, file);
  }

  /**
   * Verifies path and project path.
   *
   * @param cwd the current working directory to resolve the file to
   * @param path file or project to execute
   * @param projectPath project path or {@code null} if it hasn't been specified
   * @return tuple with boolean, File to execute and path for project to use or {@code null} if
   *     execution shall finish with an error
   */
  static scala.Tuple3<Boolean, File, String> findFileAndProject(
      String cwd, String path, String projectPath) throws IOException, ExitCode {
    var existAndFile = fileExists(cwd, path);
    LOGGER.debug(
        "findFileAndProject cwd {}, path {}, projectPath {} yields {}",
        cwd,
        path,
        projectPath,
        existAndFile);
    if (!existAndFile._1()) {
      throw new ExitCode("File " + path + " does not exist.", 1);
    }
    var file = existAndFile._2();
    var projectMode = file.isDirectory();
    var canonicalFile = file.getCanonicalFile();
    LOGGER.debug("Found file {}, project mode {}", canonicalFile, projectMode);
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
          throw new ExitCode(msg, 1);
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

  static <S> void printStackTrace(
      Iterable<S> stack,
      boolean syntaxError,
      String msg,
      File relativeTo,
      Consumer<String> print,
      Function<S, String> fnLangId,
      Function<S, String> fnRootName,
      Function<S, SourceSection> fnSourceSection) {

    var fullStackReversed = new LinkedList<S>();
    for (var e : stack) {
      fullStackReversed.addFirst(e);
    }

    var dropInitJava =
        new ArrayList<S>(
            fullStackReversed.stream()
                .dropWhile(f -> !LanguageInfo.ID.equals(fnLangId.apply(f)))
                .toList());
    Collections.reverse(dropInitJava);
    print.accept("Execution finished with an error: " + msg);

    Iterable<S> toPrint;
    if (syntaxError) {
      toPrint = null;
    } else if (dropInitJava.isEmpty()) {
      toPrint = stack;
    } else {
      toPrint = dropInitJava;
    }
    if (toPrint != null) {
      for (var f : toPrint) {
        printFrame(f, relativeTo, print, fnLangId, fnRootName, fnSourceSection);
      }
    }
  }

  private static <S> void printFrame(
      S frame,
      File relativeTo,
      Consumer<String> print,
      Function<S, String> fnLangId,
      Function<S, String> fnRootName,
      Function<S, SourceSection> fnSourceSection) {
    var langId = fnLangId.apply(frame);

    String fmtFrame;
    if (LanguageInfo.ID.equals(langId)) {
      var fName = fnRootName.apply(frame);

      var src = "Internal";
      var sourceLoc = fnSourceSection.apply(frame);
      if (sourceLoc != null) {
        var path = sourceLoc.getSource().getPath();
        var ident = sourceLoc.getSource().getName();
        if (path != null) {
          if (relativeTo != null) {
            var absRoot = relativeTo.getAbsoluteFile();
            if (path.startsWith(absRoot.getAbsolutePath())) {
              var rootDir = absRoot.isDirectory() ? absRoot : absRoot.getParentFile();
              ident = rootDir.toPath().relativize(new File(path).toPath()).toString();
            }
          }
        }

        var loc = sourceLoc.getStartLine() + "-" + sourceLoc.getEndLine();
        var line = sourceLoc.getStartLine();
        if (line == sourceLoc.getEndLine()) {
          var start = sourceLoc.getStartColumn();
          var end = sourceLoc.getEndColumn();
          loc = line + ":" + start + "-" + end;
        }
        src = ident + ":" + loc;
      }
      fmtFrame = fName + "(" + src + ")";
    } else {
      fmtFrame = frame.toString();
    }
    print.accept("        at <" + langId + "> " + fmtFrame);
  }

  /**
   * This method has to be called as early as possible. It attempts to find the project root
   * directory of the given file, and if the project root is found, it uses native code to change
   * the working directory to the project root. In order for the JVM's {@code java.io} to reflect
   * the working directory change, this methods must be called before any class from {@code java.io}
   * is accessed.
   *
   * <p>Note that invoking native code is the only reliable way to change the working directory in
   * the current process.
   *
   * <p>For detailed explanation see this <a
   * href="https://github.com/enso-org/enso/pull/12618#issuecomment-2778451448">GH comment</a>.
   *
   * @param fileToRun the file to run, value of the {@code --run} option.
   * @return cwd to use or {@code null} if no change to cwd was made
   */
  static String adjustCwdToProject(String fileToRun) {
    assert fileToRun != null;
    if (!ImageInfo.inImageRuntimeCode()) {
      return System.getProperty("enso.user.dir");
    }
    var nativeApi = WorkingDirectory.getInstance();
    var projectRoot = nativeApi.findProjectRoot(fileToRun);
    if (projectRoot != null) {
      var parentDir = nativeApi.parentFile(projectRoot);
      assert parentDir != null;
      var curDir = nativeApi.currentWorkingDir();
      if (!parentDir.equals(curDir)) {
        var dirChanged = nativeApi.changeWorkingDir(parentDir);
        if (!dirChanged) {
          LOGGER.error("Cannot change working directory to {}", parentDir);
        }
      }
      return curDir;
    } else {
      return null;
    }
  }
}
