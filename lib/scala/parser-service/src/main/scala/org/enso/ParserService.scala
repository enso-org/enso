package org.enso

import io.circe.Json
import org.enso.flexer.Reader
import org.enso.parserservice.Protocol
import org.enso.parserservice.Server
import org.enso.syntax.text.AST
import org.enso.syntax.text.SourceFile
import org.enso.syntax.text.Parser

import scala.util.Try

object ParserService {
  val HOSTNAME_VAR = "ENSO_PARSER_HOSTNAME"
  val PORT_VAR     = "ENSO_PARSER_PORT"

  val DEFAULT_PORT     = 30615
  val DEFAULT_HOSTNAME = "localhost"

  /** Obtains configuration from environment, filling missing values with
    * defaults.
    */
  def configFromEnv(): Server.Config = {
    val hostname = sys.env.getOrElse(HOSTNAME_VAR, DEFAULT_HOSTNAME)
    val port = sys.env
      .get(PORT_VAR)
      .flatMap(str => Try { str.toInt }.toOption)
      .getOrElse(DEFAULT_PORT)
    Server.Config(hostname, port)
  }
}

/** Class that allows setting up parser service with given configuration. */
case class ParserService() extends Server with Protocol {
  import parserservice._
  import Protocol._

  def serializeAst(ast: AST.Module): String = ast.toJson().noSpaces

  def handleRequest(request: Request): Response = {
    request match {
      case ParseRequest(program, ids) =>
        val ast = new Parser().run(new Reader(program), ids)
        Protocol.Success(SourceFile(ast, Json.Null))
      case ParseRequestWithMetadata(content) =>
        val module = new Parser().runWithMetadata(content)
        Protocol.Success(module)
      case _ =>
        throw new Exception(f"unimplemented request: $request")
    }
  }
}

/** Runs a simple WebSocket server that wraps Parser into a service. */
object ParserServiceMain extends App {
  import ParserService._
  println("Getting configuration from environment...")
  val config = configFromEnv()

  println(s"Will serve ${config.addressString()}")
  println(
    s"To change configuration, restart with $HOSTNAME_VAR or " +
    s"$PORT_VAR variables set to desired values"
  )
  val service = ParserService()
  service.start(config)
}
