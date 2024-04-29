package org.enso.runner

import java.util.UUID
import java.nio.file.Paths
import org.apache.commons.cli.CommandLine
import cats.implicits._
import org.enso.languageserver.boot.{
  LanguageServerConfig,
  ProfilingConfig,
  StartupConfig
}
import org.slf4j.event.Level
import org.enso.common.{HostEnsoUtils, LanguageInfo}
import org.graalvm.polyglot.PolyglotException

import java.io.File

import scala.concurrent.duration._
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

  /** Handles `--server` CLI option
    *
    * @param line     a CLI line
    * @param logLevel log level to set for the engine runtime
    */
  final def runLanguageServer(line: CommandLine, logLevel: Level): Unit = {
    val maybeConfig = parseServerOptions(line)

    maybeConfig match {
      case Left(errorMsg) =>
        System.err.println(errorMsg)
        Main.exitFail()

      case Right(config) =>
        LanguageServerApp.run(
          config,
          logLevel,
          line.hasOption(Main.DAEMONIZE_OPTION)
        )
        Main.exitSuccess()
    }
  }

  final def parseServerOptions(
    line: CommandLine
  ): Either[String, LanguageServerConfig] =
    for {
      rootId <- Option(line.getOptionValue(Main.ROOT_ID_OPTION))
        .toRight("Root id must be provided")
        .flatMap { id =>
          Either
            .catchNonFatal(UUID.fromString(id))
            .leftMap(_ => "Root must be UUID")
        }
      rootPath <- Option(line.getOptionValue(Main.ROOT_PATH_OPTION))
        .toRight("Root path must be provided")
      interface = Option(line.getOptionValue(Main.INTERFACE_OPTION))
        .getOrElse("127.0.0.1")
      rpcPortStr = Option(line.getOptionValue(Main.RPC_PORT_OPTION))
        .getOrElse("8080")
      rpcPort <- Either
        .catchNonFatal(rpcPortStr.toInt)
        .leftMap(_ => "Port must be integer")
      dataPortStr = Option(line.getOptionValue(Main.DATA_PORT_OPTION))
        .getOrElse("8081")
      dataPort <- Either
        .catchNonFatal(dataPortStr.toInt)
        .leftMap(_ => "Port must be integer")
      secureRpcPortStr = Option(
        line.getOptionValue(Main.SECURE_RPC_PORT_OPTION)
      )
        .map(Some(_))
        .getOrElse(None)
      secureRpcPort <- Either
        .catchNonFatal(secureRpcPortStr.map(_.toInt))
        .leftMap(_ => "Port must be integer")
      secureDataPortStr = Option(
        line.getOptionValue(Main.SECURE_DATA_PORT_OPTION)
      )
        .map(Some(_))
        .getOrElse(None)
      secureDataPort <- Either
        .catchNonFatal(secureDataPortStr.map(_.toInt))
        .leftMap(_ => "Port must be integer")
      profilingConfig <- parseProfilingConfig(line)
      graalVMUpdater = Option(line.hasOption(Main.SKIP_GRAALVM_UPDATER))
        .getOrElse(false)
    } yield LanguageServerConfig(
      interface,
      rpcPort,
      secureRpcPort,
      dataPort,
      secureDataPort,
      rootId,
      rootPath,
      profilingConfig,
      StartupConfig(graalVMUpdater)
    )

  final def parseProfilingConfig(
    line: CommandLine
  ): Either[String, ProfilingConfig] = {
    val profilingPathStr =
      Option(line.getOptionValue(Main.PROFILING_PATH))
    for {
      profilingPath <- Either
        .catchNonFatal(profilingPathStr.map(Paths.get(_)))
        .leftMap(_ => "Profiling path is invalid")
      profilingTimeStr = Option(
        line.getOptionValue(Main.PROFILING_TIME)
      )
      profilingTime <- Either
        .catchNonFatal(profilingTimeStr.map(_.toInt.seconds))
        .leftMap(_ => "Profiling time should be an integer")
    } yield ProfilingConfig(profilingPath, profilingTime)
  }
}
