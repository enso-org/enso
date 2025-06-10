package org.enso.projectmanager.boot.configuration

/** A configuration object for properties of the JSON RPC server.
  *
  * @param host an address that the server listen on
  * @param port a port that the server listen on
  */
case class ServerConfig(host: String, port: Int)
