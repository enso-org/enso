package org.enso.languageserver.runtime

import org.enso.languageserver.runtime.ExecutionApi.ContextId

object ExecutionProtocol {

  sealed trait Api

  /**
    * A request to the language server to create a new execution context.
    *
    * @param contextId the newly created context's id
    */
  case class CreateContextRequest(contextId: ContextId) extends Api

  /**
    * A response about creation of a new execution context.
    *
    * @param contextId the newly created context's id
    */
  case class CreateContextResponse(contextId: ContextId) extends Api

}
