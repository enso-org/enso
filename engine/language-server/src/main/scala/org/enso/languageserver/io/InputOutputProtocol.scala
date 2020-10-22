package org.enso.languageserver.io

import org.enso.languageserver.data.ClientId

object InputOutputProtocol {

  /** A command to the output controller responsible for redirecting an output.
    *
    * @param clientId the client that the redirection is performed for
    */
  case class RedirectOutput(clientId: ClientId)

  /** A command to the output controller responsible for suppressing a
    * redirection of an output.
    *
    * @param clientId the client that the suppression is performed for
    */
  case class SuppressOutput(clientId: ClientId)

  /** A notification that signals that new output was appended to the stdout or
    * stderr.
    *
    * @param output the new data
    * @param outputKind a kind of the output
    */
  case class OutputAppended(output: String, outputKind: OutputKind)

  /** A command that feeds the standard input.
    *
    * @param input the data that feeds stdin
    * @param isLineTerminated signals if the input is terminated with a line
    *                         separator
    */
  case class FeedStandardInput(input: String, isLineTerminated: Boolean)

  /** Signals that a user program is blocked by `IO.readln` operation.
    */
  case object WaitingForStandardInput

}
