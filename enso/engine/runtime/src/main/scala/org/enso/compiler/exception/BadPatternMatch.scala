package org.enso.compiler.exception

/** An error representing a bad pattern match. */
sealed trait BadPatternMatch {
  val message: String
}
object BadPatternMatch {

  /** Where the pattern match has the wrong number of arguments for the
    * constructor.
    *
    * @param name the name of the requested constructor
    * @param expected the number of arguments expected by the constructor
    * @param provided the number of arguments provided at the match site
    */
  sealed case class WrongArgCount(name: String, expected: Int, provided: Int)
      extends BadPatternMatch {
    val fieldWord = if (provided == 1) "field" else "fields"

    override val message: String =
      s"Cannot match on $name using $provided $fieldWord (expecting $expected)"
  }

  /** Where the pattern match is matching on a constructor not visible in the
    * current scope.
    *
    * @param name the name of the requested constructor
    */
  sealed case class NonVisibleConstructor(name: String)
      extends BadPatternMatch {
    override val message: String = s"$name is not visible in this scope"
  }
}
