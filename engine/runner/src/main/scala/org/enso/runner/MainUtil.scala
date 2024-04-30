package org.enso.runner

import org.enso.common.{HostEnsoUtils, LanguageInfo}
import org.graalvm.polyglot.PolyglotException

import java.io.File

import scala.jdk.CollectionConverters._

/** Remaining utilities for the Main CLI class. */
private[runner] object MainUtil {
  final def printPolyglotException(
    exception: PolyglotException,
    relativeTo: Option[File]
  ): Unit = {
    val fullStack = exception.getPolyglotStackTrace.asScala.toList
    val dropInitJava = fullStack.reverse
      .dropWhile(_.getLanguage.getId != LanguageInfo.ID)
      .reverse
    val msg: String = HostEnsoUtils.findExceptionMessage(exception)
    println(s"Execution finished with an error: ${msg}")
    def printFrame(frame: PolyglotException#StackFrame): Unit = {
      val langId =
        if (frame.isHostFrame) "java" else frame.getLanguage.getId
      val fmtFrame = if (frame.getLanguage.getId == LanguageInfo.ID) {
        val fName = frame.getRootName
        val src = Option(frame.getSourceLocation)
          .map { sourceLoc =>
            val ident = Option(sourceLoc.getSource.getPath)
              .map { path =>
                relativeTo match {
                  case None => path
                  case Some(root) =>
                    val absRoot = root.getAbsoluteFile
                    if (path.startsWith(absRoot.getAbsolutePath)) {
                      val rootDir =
                        if (absRoot.isDirectory) absRoot
                        else absRoot.getParentFile
                      rootDir.toPath.relativize(new File(path).toPath).toString
                    } else {
                      path
                    }
                }
              }
              .getOrElse(sourceLoc.getSource.getName)
            val loc = if (sourceLoc.getStartLine == sourceLoc.getEndLine) {
              val line  = sourceLoc.getStartLine
              val start = sourceLoc.getStartColumn
              val end   = sourceLoc.getEndColumn
              s"$line:$start-$end"
            } else {
              s"${sourceLoc.getStartLine}-${sourceLoc.getEndLine}"
            }
            s"$ident:$loc"
          }
          .getOrElse("Internal")
        s"$fName($src)"
      } else {
        frame.toString
      }
      println(s"        at <$langId> $fmtFrame")
    }
    if (exception.isSyntaxError()) {
      // no stack
    } else if (dropInitJava.isEmpty) {
      fullStack.foreach(printFrame)
    } else {
      dropInitJava.foreach(printFrame)
    }
  }
}
