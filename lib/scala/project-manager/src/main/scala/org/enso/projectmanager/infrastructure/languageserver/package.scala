package org.enso.projectmanager.infrastructure

package object languageserver {

  /** A stop command. */
  case object GracefulStop

  /** Requests to restart the language server. */
  case object Restart
}
