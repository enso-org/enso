package org.enso.runner;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.function.Consumer;
import java.util.function.Function;
import org.enso.common.LanguageInfo;
import org.graalvm.polyglot.SourceSection;
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
}
