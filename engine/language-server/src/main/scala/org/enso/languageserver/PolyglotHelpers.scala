package org.enso.languageserver
import org.enso.interpreter.Constants
import org.graalvm.polyglot.{Context, Value}

object PolyglotHelpers {

  /**
    * @param context the current language context
    * @return the top scope of Enso execution context
    */
  def getTopScope(context: Context): TopScope =
    new TopScope(context.getBindings(Constants.LANGUAGE_ID))

  /**
    * Represents the top scope of Enso execution context
    *
    * @param value the polyglot value of this scope
    */
  class TopScope(private val value: Value) {

    /** Gets a module by name.
      *
      * @param name the module name to get
      * @return the representation of the module
      */
    def getModule(name: String): Module =
      new Module(value.invokeMember("get_module", name))

    /**
      * Creates a new, unregistered module.
      *
      * The returned module is not accessible through [[getModule()]].
      *
      * @param name the name for the new module
      * @return a new module
      */
    def createModule(name: String): Module =
      new Module(value.invokeMember("create_module", name))
  }

  /**
    * Represents an Enso Module.
    *
    * @param value the polyglot value of this scope
    */
  class Module(private val value: Value) {

    /**
      * @return the associated type of this module
      */
    def getAssociatedConstructor: Value =
      value.invokeMember("get_associated_constructor")

    /**
      * Gets a method by the type it's defined on and name.
      *
      * @param constructor the constructor the method is defined on
      * @param name the name of the method
      * @return the runtime representation of the method
      */
    def getMethod(constructor: Value, name: String): Function =
      new Function(value.invokeMember("get_method", constructor, name))

    /**
      * Parses additional source code in the context of this module.
      *
      * Updates the module with any new methods and imports from the new
      * source.
      *
      * @param additionalSource the new source to parse
      */
    def patch(additionalSource: String): Unit =
      value.invokeMember("patch", additionalSource)
  }

  /**
    * Represents an Enso function.
    *
    * @param value the polyglot value of this function
    */
  class Function(private val value: Value) {

    /**
      * Executes the function with given parameters.
      * @param args the execution arguments
      * @return the result of execution
      */
    def execute(args: Object*): Value = value.execute(args: _*)

    /**
      * Checks function equality by checking the identity of the underlying
      * objects.
      *
      * @param obj the other comparison operand
      * @return `true` if the functions correspond to the same underlying object,
      *        `false` otherwise.
      */
    override def equals(obj: Any): Boolean = obj match {
      case fun: Function => value.invokeMember("equals", fun.value).asBoolean()
      case _             => false
    }
  }
}
