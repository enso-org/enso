package org.enso.languageserver.data

/**
  * The state of the running Language Server instance.
  *
  * @param clients the list of currently connected clients.
  */
case class Environment(clients: List[Client]) {

  /**
    * Adds a new client to this `Env`
    * @param client the client to add.
    * @return a new version of the environment with the client added.
    */
  def addClient(client: Client): Environment = {
    copy(clients = client :: clients)
  }

  /**
    * Removes a client by id.
    *
    * @param clientId the id of the client to remove.
    * @return a new version of the environment with the client removed.
    */
  def removeClient(clientId: Client.Id): Environment =
    copy(clients = clients.filter(_.id != clientId))

}

object Environment {

  /**
    * Constructs an empty env.
    *
    * @return an empty env.
    */
  def empty: Environment = Environment(List())
}
