package org.enso.compiler.pass

import org.enso.compiler.pass.PassConfiguration.ConfigMap
import shapeless.=:!=

import scala.annotation.unused

/** Stores configuration for the various compiler passes.
  *
  * @param config the initial pass configurations
  */
class PassConfiguration(
  config: ConfigMap = Map()
) {
  private var configuration: ConfigMap = config

  /** Adds a new configuration entity to the pass configuration, or updates it
    * if it already exists for a given pass.
    *
    * @param pass the pass to add the configuration for
    * @param config the configuration to add
    */
  def update(pass: IRPass, config: IRPass.Configuration): Unit = {
    configuration = configuration + (pass -> config)
  }

  /** Removes the configuration for the specified pass from the list.
    *
    * @param pass the pass to remove configuration for
    * @return the removed configuration for that pass, if it exists
    */
  def remove(pass: IRPass): Option[IRPass.Configuration] = {
    configuration.get(pass) match {
      case res @ Some(_) =>
        configuration = configuration.filterNot(pair => pair._1 == pass)
        res
      case None => None
    }
  }

  /** Gets the configuration for the specified pass.
    *
    * The output cast is intentionally unchecked, so a bad cast will be a
    * compiler bug.
    *
    * @param pass the pass to get the configuration for
    * @tparam T the expected type of the configuration, must be provided
    * @return the configuration for `pass`, if it exists
    */
  def get[T <: IRPass.Configuration](
    pass: IRPass
  )(implicit @unused ev: T =:!= IRPass.Configuration): Option[T] = {
    configuration.get(pass).map(_.asInstanceOf[T])
  }
}
object PassConfiguration {
  type ConfigMap = Map[IRPass, IRPass.Configuration]
}