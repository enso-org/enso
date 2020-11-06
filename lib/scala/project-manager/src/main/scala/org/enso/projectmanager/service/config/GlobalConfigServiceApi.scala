package org.enso.projectmanager.service.config

trait GlobalConfigServiceApi[F[+_, +_]] {
  def getKey(key: String):                F[GlobalConfigServiceFailure, Option[String]]
  def setKey(key: String, value: String): F[GlobalConfigServiceFailure, Unit]
  def deleteKey(key: String):             F[GlobalConfigServiceFailure, Unit]
}
