package org.enso.runner

import java.io.{InputStream, OutputStream, PrintStream, PrintWriter, Writer}
import java.util.Scanner

import org.enso.polyglot.debugger.{ReplExecutor, SessionManager}
import org.jline.reader.impl.DefaultParser
import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.{Terminal, TerminalBuilder}

import scala.util.Try

/**
  * Represents user console input.
  */
sealed trait UserInput

/**
  * End of user input.
  */
case object EndOfInput extends UserInput

/**
  * A normal line of user input.
  * @param line the contents of the input line
  */
case class Line(line: String) extends UserInput

/**
  * Abstract representation of Repl IO operations
  */
trait ReplIO {

  /**
    * Ask user for a line of input, using given prompt
    * @param prompt the prompt to display to the user
    * @return the user-provided input
    */
  def readLine(prompt: String): UserInput

  /**
    * Print a line to the REPL.
    * @param contents contents of the line to print
    */
  def println(contents: String): Unit

  /**
    * Print a stack trace to the REPL.
    * @param exception which stack trace is to be printed
    */
  def printStackTrace(exception: Exception): Unit = {
    val traceBuilder = new StringBuilder
    val traceWriter = new Writer() {
      override def write(
        cbuf: Array[Char],
        off: Int,
        len: Int
      ): Unit =
        traceBuilder.append(cbuf.slice(off, off + len).mkString)

      override def flush(): Unit = {}

      override def close(): Unit = {}
    }
    exception.printStackTrace(new PrintWriter(traceWriter))
    println(traceBuilder.toString())
  }
}

/**
  * A barebones implementation of [[ReplIO]] based on standard input / output operations.
  * @param in input stream to use
  * @param out output stream to use
  */
case class SimpleReplIO(in: InputStream, out: OutputStream) extends ReplIO {
  private val scanner: Scanner     = new Scanner(in)
  private val printer: PrintStream = new PrintStream(out)

  /**
    * Ask user for a line of input, using given prompt
    * @param prompt the prompt to display to the user
    * @return the user-provided input
    */
  override def readLine(prompt: String): UserInput = {
    printer.print(prompt)
    Try(scanner.nextLine()).map(Line).getOrElse(EndOfInput)
  }

  /**
    * Print a line to the REPL.
    * @param contents contents of the line to print
    */
  override def println(contents: String): Unit = printer.println(contents)
}

/**
  * An implementation of [[ReplIO]] using system terminal capabilities.
  */
case class TerminalIO() extends ReplIO {
  private val terminal: Terminal =
    TerminalBuilder.builder().system(true).build()
  private val parser: DefaultParser = new DefaultParser()
  parser.setEscapeChars(null)
  private val lineReader: LineReader =
    LineReaderBuilder.builder().parser(parser).terminal(terminal).build()

  /**
    * Ask user for a line of input, using given prompt
    * @param prompt the prompt to display to the user
    * @return the user-provided input
    */
  override def readLine(prompt: String): UserInput =
    Try(lineReader.readLine(prompt)).map(Line).getOrElse(EndOfInput)

  /**
    * Print a line to the REPL.
    * @param contents contents of the line to print
    */
  override def println(contents: String): Unit =
    terminal.writer().println(contents)
}

/**
  * The Repl logic to inject into runtime instrumentation framework.
  *
  * @param replIO the IO implementation to use with this Repl
  */
case class Repl(replIO: ReplIO) extends SessionManager {

  /**
    * Runs the Repl session by asking for user input and performing
    * the requested action with the execution node.
    *
    * End of input causes exit from the Repl.
    *
    * @param executor the interface for executing commands inside the session
    */
  override def startSession(
    executor: ReplExecutor
  ): Nothing = {
    var continueRunning = true
    while (continueRunning) {
      val input = replIO.readLine("> ")
      input match {
        case EndOfInput =>
          continueRunning = false
        case Line(line) =>
          if (line == ":list" || line == ":l") {
            val bindings = executor.listBindings()
            bindings.foreach {
              case (varName, value) =>
                replIO.println(s"$varName = $value")
            }
          } else if (line == ":quit" || line == ":q") {
            continueRunning = false
          } else {
            val result = executor.evaluate(line)
            result match {
              case Left(exception) =>
                replIO.println(
                  s"Evaluation failed with: ${exception.getMessage}"
                )
                replIO.printStackTrace(exception)
              case Right(objectRepresentation) =>
                replIO.println(s">>> $objectRepresentation")
            }
          }
      }
    }

    executor.exit()
  }
}
