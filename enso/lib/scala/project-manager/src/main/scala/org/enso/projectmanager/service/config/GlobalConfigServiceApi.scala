package org.enso.projectmanager.service.config

import nl.gn0s1s.bump.SemVer
import org.enso.editions.EnsoVersion

/** A contract for the Global Config Service.
  *
  * @tparam F a monadic context
  */
trait GlobalConfigServiceApi[F[+_, +_]] {

  /** Gets a value from the global config under the given key.
    *
    * Returns None if the key is not present in the config.
    */
  def getKey(key: String): F[GlobalConfigServiceFailure, Option[String]]

  /** Sets a value for the specified key in the global config.
    *
    * It updates or inserts the value.
    */
  def setKey(key: String, value: String): F[GlobalConfigServiceFailure, Unit]

  /** Deletes a key from the global config.
    *
    * If the value was not present already, nothing happens.
    */
  def deleteKey(key: String): F[GlobalConfigServiceFailure, Unit]

  /** Returns the default engine version.
    *
    * It reads the setting from the config, or if no version is set, falls back
    * to the latest installed (or latest available if none are installed)
    * version.
    */
  def getDefaultEnsoVersion: F[GlobalConfigServiceFailure, SemVer]

  /** Resolves an [[EnsoVersion]] which can indicate to use a 'default' version
    * to a concrete version.
    */
  def resolveEnsoVersion(
    ensoVersion: EnsoVersion
  ): F[GlobalConfigServiceFailure, SemVer]
}
