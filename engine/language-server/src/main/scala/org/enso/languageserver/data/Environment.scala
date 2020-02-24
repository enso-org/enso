package org.enso.languageserver.data

/**
  * The config of the running Language Server instance.
  *
  * Currently empty, to be filled in with content roots etc.
  */
case class Config()

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

  /**
    * Removes all registered capabilities matching a given predicate.
    *
    * @param predicate the predicate to match capabilities against.
    * @return a new version of `Env` without the capabilities matching the
    *         predicate and a list of all clients, together with capabilities
    *         that got removed for them.
    */
  def removeCapabilitiesBy(
    predicate: CapabilityRegistration => Boolean
  ): (Environment, List[(Client, List[CapabilityRegistration])]) = {
    val newClients = clients.map { client =>
      val (removedCapabilities, retainedCapabilities) =
        client.capabilities.partition(predicate)
      val newClient = client.copy(capabilities = retainedCapabilities)
      (newClient, removedCapabilities)
    }
    (copy(clients = newClients.map(_._1)), newClients)
  }

  /**
    * Modified a client at a given id.
    *
    * @param clientId the id of the client to modify.
    * @param modification the function used to modify the client.
    * @return a new version of this env, with the selected client modified by
    *         `modification`
    */
  def modifyClient(
    clientId: Client.Id,
    modification: Client => Client
  ): Environment = {
    val newClients = clients.map { client =>
      if (client.id == clientId) {
        modification(client)
      } else {
        client
      }
    }
    copy(clients = newClients)
  }

  /**
    * Grants a given client a provided capability.
    *
    * @param clientId the id of the client to grant the capability.
    * @param registration the capability to grant.
    * @return a new version of this env, with the capability granted.
    */
  def grantCapability(
    clientId: Client.Id,
    registration: CapabilityRegistration
  ): Environment =
    modifyClient(clientId, { client =>
      client.copy(capabilities = registration :: client.capabilities)
    })

  /**
    * Releases a capability from a given client.
    *
    * @param clientId the id of the client that releases the capability.
    * @param capabilityId the id of the capability registration to release.
    * @return a new version of this env, with the selected capability released.
    */
  def releaseCapability(
    clientId: Client.Id,
    capabilityId: CapabilityRegistration.Id
  ): Environment =
    modifyClient(clientId, { client =>
      client.copy(
        capabilities = client.capabilities.filter(_.id != capabilityId)
      )
    })
}

object Environment {

  /**
    * Constructs an empty env.
    *
    * @return an empty env.
    */
  def empty: Environment = Environment(List())
}
