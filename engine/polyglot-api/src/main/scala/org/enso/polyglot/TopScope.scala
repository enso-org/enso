package org.enso.polyglot

import org.graalvm.polyglot.Value

/** Represents the top scope of Enso execution context
  *
  * @param value the polyglot value of this scope
  */
class TopScope(private val value: Value) {
  import MethodNames.TopScope._

  /** Gets a module by name.
    *
    * @param name the module name to get
    * @return the representation of the module
    */
  def getModule(name: String): Module =
    new Module(value.invokeMember(GET_MODULE, name))

  /** Creates a new, unregistered module.
    *
    * The returned module is not accessible through [[getModule()]].
    *
    * @param name the name for the new module
    * @return a new module
    */
  def createModule(name: String): Module =
    new Module(value.invokeMember(CREATE_MODULE, name))

  def registerModule(qualifiedName: String, filePath: String): Module =
    new Module(value.invokeMember(REGISTER_MODULE, qualifiedName, filePath))

  def unregisterModule(qualifiedName: String): Unit = {
    value.invokeMember(UNREGISTER_MODULE, qualifiedName): Unit
  }

  def compile(shouldCompileDependencies: Boolean): Unit = {
    value.invokeMember(COMPILE, shouldCompileDependencies)
  }
}
