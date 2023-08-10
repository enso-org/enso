package org.enso.pkg.validation

import scala.collection.immutable.ListSet

object NameValidation {

  /** Transforms the given string into a valid package name (i.e. a CamelCased identifier).
    *
    * @param name the original name.
    * @return the transformed name conforming to the specification.
    */
  def normalizeName(name: String): String = {
    val starting =
      if (
        name.isEmpty ||
        name.filter(_ != '_').forall(c => !isAllowedNameCharacter(c))
      ) "Project"
      else if (!name.head.isLetter) "Project_" ++ name
      else name
    val startingWithUppercase = starting.capitalize
    val onlyAlphanumeric      = startingWithUppercase.filter(isAllowedNameCharacter)
    toUpperSnakeCase(onlyAlphanumeric)
  }

  /** Validate the project name.
    *
    * @param name the project name to validate
    * @return either a validation error or a project name if it's valid
    */
  def validateName(name: String): Either[InvalidNameError, String] =
    if (name.isEmpty) {
      Left(InvalidNameError.Empty)
    } else if (!name.head.isLetter || !name.head.isUpper) {
      Left(InvalidNameError.ShouldStartWithCapitalLetter)
    } else if (!name.forall(isAllowedNameCharacter)) {
      val invalidCharacters = name.filterNot(isAllowedNameCharacter)
      Left(
        InvalidNameError.ContainsInvalidCharacters(
          ListSet(invalidCharacters: _*)
        )
      )
    } else if (name != toUpperSnakeCase(name)) {
      Left(InvalidNameError.ShouldBeUpperSnakeCase(toUpperSnakeCase(name)))
    } else {
      Right(name)
    }

  /** Checks if a character is allowed in a project name.
    *
    * @param char the char to validate
    * @return `true` if it's allowed, `false` otherwise
    */
  private def isAllowedNameCharacter(char: Char): Boolean = {
    char.isLetterOrDigit || char == '_'
  }

  /** Takes a name containing letters, digits, and `_` characters and makes it
    * a proper `Upper_Snake_Case` name.
    *
    * @param string the input string
    * @return the transformed string
    */
  private def toUpperSnakeCase(string: String): String = {
    val beginMarker = '#'
    val chars       = string.toList
    val charPairs   = (beginMarker :: chars).zip(chars)
    charPairs
      .map { case (previous, current) =>
        if (previous == beginMarker) {
          current.toString
        } else if (previous.isLower && current.isUpper) {
          s"_$current"
        } else if (previous.isLetter && current.isDigit) {
          s"_$current"
        } else if (previous == '_' && current == '_') {
          ""
        } else if (previous.isDigit && current.isLetter) {
          s"_${current.toUpper}"
        } else {
          current.toString
        }
      }
      .mkString("")
  }

}
