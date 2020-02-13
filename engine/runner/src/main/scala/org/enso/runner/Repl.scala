package org.enso.runner

import java.io.{InputStream, OutputStream, PrintStream}
import java.util.Scanner

import org.enso.interpreter.instrument.ReplDebuggerInstrument
import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.{Terminal, TerminalBuilder}

import scala.jdk.CollectionConverters._
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
  private val lineReader: LineReader =
    LineReaderBuilder.builder().terminal(terminal).build()

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
case class Repl(replIO: ReplIO) extends ReplDebuggerInstrument.SessionManager {

  /**
    * Runs the Repl session by asking for user input and performing
    * the requested action with the execution node.
    *
    * End of input causes exit from the Repl.
    *
    * @param executionNode the execution node capable of performing
    *                      language-level operations
    */
  override def startSession(
    executionNode: ReplDebuggerInstrument.ReplExecutionEventNode
  ): Unit = {
    while (true) {
      val input = replIO.readLine("> ")
      input match {
        case EndOfInput =>
          executionNode.exit()
          return
        case Line(line) =>
          if (line == ":list" || line == ":l") {
            val bindings = executionNode.listBindings().asScala
            bindings.foreach {
              case (varName, value) =>
                replIO.println(s"$varName = $value")
            }
          } else if (line == ":quit" || line == ":q") {
            executionNode.exit()
            return
          } else {
            replIO.println(s">>> ${executionNode.evaluate(line)}")
          }
      }
    }
  }
}
