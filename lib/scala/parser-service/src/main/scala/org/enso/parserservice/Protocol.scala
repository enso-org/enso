package org.enso.parserservice

import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import org.enso.syntax.text.Parser
import org.enso.syntax.text.SourceFile

/** Types implementing parser server protocol.
  *
  * The protocol always is single request -> single response.
  */
object Protocol {
  sealed trait Request
  final case class ParseRequest(program: String, ids: Parser.IDMap)
      extends Request
  final case class ParseRequestWithMetadata(content: String)    extends Request
  final case class DocParserGenerateHtmlSource(program: String) extends Request
  final case class DocParserGenerateHtmlFromDoc(code: String)   extends Request

  sealed trait Response
  final case class Success(module: SourceFile) extends Response
  final case class Error(message: String)      extends Response
  final case class SuccessDoc(code: String)    extends Response
}

/** Helper for implementing protocol over text-based transport.
  *
  * Requests and responses are marshaled as text using JSON
  * (and default circe serialzation schema).
  */
trait Protocol {
  import Protocol._

  /** Generate [[Response]] for a given [[Request]].
    *
    * Any [[Throwable]] thrown out of implementation will be translated into
    * [[Protocol.Error]] message.
    */
  def handleRequest(request: Request): Response

  def handleMessage(input: String): String = {
    try {
      decode[Request](input) match {
        case Left(err)      => throw err
        case Right(request) => handleRequest(request).asJson.noSpaces
      }
    } catch {
      case e: Throwable => (Error(e.toString): Response).asJson.noSpaces
    }
  }
}
