package org.enso.gateway

import org.enso.gateway.JsonRpcController.jsonRpcVersion
import org.enso.gateway.protocol.{Notification, Notifications}
import org.enso.gateway.protocol.request.Params
import org.enso.gateway.protocol.request.Params.VoidParams

trait TestNotification[P <: Params] {
  def notification: Notification[P]
}

object TestNotification {

  object Initialized extends TestNotification[VoidParams] {
    val notification = Notification(
      jsonrpc = jsonRpcVersion,
      method  = Notifications.Initialized.method,
      params  = Some(VoidParams())
    )
  }

  object Exit extends TestNotification[VoidParams] {
    val notification = Notification(
      jsonrpc = jsonRpcVersion,
      method  = Notifications.Exit.method,
      params  = Some(VoidParams())
    )
  }

}
