package org.enso.searcher

/** A search suggestion. */
sealed trait Suggestion
object Suggestion {

  /** The type of a suggestion. */
  sealed trait Kind
  object Kind {

    /** The atom suggestion. */
    case object Atom extends Kind

    /** The method suggestion. */
    case object Method extends Kind

    /** The function suggestion. */
    case object Function extends Kind

    /** The suggestion of a local value. */
    case object Local extends Kind
  }

  /** An argument of an atom or a function.
    *
    * @param name the argument name
    * @param reprType the type of the argument
    * @param isSuspended is the argument lazy
    * @param hasDefault does the argument have a default
    * @param defaultValue optional default value
    */
  case class Argument(
    name: String,
    reprType: String,
    isSuspended: Boolean,
    hasDefault: Boolean,
    defaultValue: Option[String]
  )

  /** The definition scope.
    * @param start the start of the definition scope
    * @param end the end of the definition scope
    */
  case class Scope(start: Int, end: Int)

  /** A value constructor.
    *
    * @param name the atom name
    * @param arguments the list of arguments
    * @param returnType the type of an atom
    * @param documentation the documentation string
    */
  case class Atom(
    name: String,
    arguments: Seq[Argument],
    returnType: String,
    documentation: Option[String]
  ) extends Suggestion

  /** A function defined on a type or a module.
    *
    * @param name the method name
    * @param arguments the function arguments
    * @param selfType the self type of a method
    * @param returnType the return type of a method
    * @param documentation the documentation string
    */
  case class Method(
    name: String,
    arguments: Seq[Argument],
    selfType: String,
    returnType: String,
    documentation: Option[String]
  ) extends Suggestion

  /** A local function definition.
    *
    * @param name the function name
    * @param arguments the function arguments
    * @param returnType the return type of a function
    * @param scope the scope where the function is defined
    */
  case class Function(
    name: String,
    arguments: Seq[Argument],
    returnType: String,
    scope: Scope
  ) extends Suggestion

  /** A local value.
    *
    * @param name the name of a value
    * @param returnType the type of a local value
    * @param scope the scope where the value is defined
    */
  case class Local(name: String, returnType: String, scope: Scope)
      extends Suggestion
}
