package org.enso.languageserver.libraries

import org.enso.editions.LibraryName
import org.enso.pkg.{ComponentGroup, ComponentGroups, Config, GroupReference}

import scala.collection.mutable

/** Validate the component groups of provided packages. */
final class ComponentGroupsValidator {

  import ComponentGroupsValidator.{ValidationError, Validator}

  /** Run the validation.
    *
    * The algorithm checks that the provided component groups are consistent:
    * - Package configs have valid component groups structure
    * - Packages don't define duplicate component groups
    * - Packages override existing component groups
    *
    * @param packages the list of package configs
    * @return the validation result for each package
    */
  def validate(
    packages: Iterable[Config]
  ): Iterable[(LibraryName, Either[ValidationError, ComponentGroups])] = {
    val groupsMap: mutable.Map[GroupReference, ComponentGroup] = mutable.Map()
    val init = packages.map { config =>
      val libraryName = LibraryName(config.namespace, config.name)
      libraryName -> validateInvalidComponentGroups(config)
    }

    runValidation(init)(
      validateDuplicateComponentGroups(groupsMap),
      validateComponentGroupExtendsNothing(groupsMap)
    )
  }

  /** Run the validation.
    *
    * @param componentGroups the component groups mapping
    * @return the validation result for each package
    */
  def validate(
    componentGroups: Map[LibraryName, ComponentGroups]
  ): Map[LibraryName, Either[ValidationError, ComponentGroups]] = {
    val init: Map[LibraryName, Either[ValidationError, ComponentGroups]] =
      componentGroups.map { case (k, v) => k -> Right(v) }
    val groupsMap: mutable.Map[GroupReference, ComponentGroup] =
      mutable.HashMap()

    runValidation(init)(
      validateDuplicateComponentGroups(groupsMap),
      validateComponentGroupExtendsNothing(groupsMap)
    ).toMap
  }

  private def validateInvalidComponentGroups(
    config: Config
  ): Either[ValidationError, ComponentGroups] = {
    val libraryName = LibraryName(config.namespace, config.name)
    config.componentGroups.left.map { e =>
      ValidationError.InvalidComponentGroups(libraryName, e.getMessage())
    }
  }

  private def validateDuplicateComponentGroups(
    groupsMap: mutable.Map[GroupReference, ComponentGroup]
  ): Validator = { libraryName => componentGroups =>
    componentGroups.newGroups
      .map { componentGroup =>
        val groupReference =
          GroupReference(libraryName, componentGroup.group)
        if (groupsMap.contains(groupReference)) {
          Left(
            ValidationError
              .DuplicatedComponentGroup(libraryName, groupReference)
          )
        } else {
          groupsMap += groupReference -> componentGroup
          Right(componentGroups)
        }
      }
      .find(_.isLeft)
      .getOrElse(Right(componentGroups))
  }

  private def validateComponentGroupExtendsNothing(
    groupsMap: mutable.Map[GroupReference, ComponentGroup]
  ): Validator = { libraryName => componentGroups =>
    componentGroups.extendedGroups
      .map { extendedComponentGroup =>
        if (groupsMap.contains(extendedComponentGroup.group)) {
          Right(componentGroups)
        } else {
          Left(
            ValidationError.ComponentGroupExtendsNothing(
              libraryName,
              extendedComponentGroup.group
            )
          )
        }
      }
      .find(_.isLeft)
      .getOrElse(Right(componentGroups))
  }

  /** Internal method that runs validation functions. The validation runs on
    * the collection on key-value pairs, where the value of the collection is
    * validated and contains either a validated value or a validation error.
    *
    * @param init the collection of key-value pairs that should be validated
    * @param validators the list of validation functions
    * @tparam K the type of keys in the validated collection
    * @tparam V the type of values in the validated collection
    * @tparam E the type of validation error
    * @return the validated collection of key-value pairs
    */
  private def runValidation[K, V, E](init: Iterable[(K, Either[E, V])])(
    validators: K => V => Either[E, V]*
  ): Iterable[(K, Either[E, V])] =
    validators.foldLeft(init) { (acc, validator) =>
      acc.map { case (k, ev) => k -> ev.flatMap(validator(k)) }
    }
}

object ComponentGroupsValidator {

  type Validator =
    LibraryName => ComponentGroups => Either[ValidationError, ComponentGroups]

  /** Base trait for validation results. */
  sealed trait ValidationError
  object ValidationError {

    /** An error indicating that the package config defines duplicate component
      * group.
      *
      * @param libraryName the library defining duplicate component group
      * @param groupReference the duplicated component group reference
      */
    case class DuplicatedComponentGroup(
      libraryName: LibraryName,
      groupReference: GroupReference
    ) extends ValidationError

    /** An error indicating that the package config has invalid component groups
      * format.
      *
      * @param libraryName the library name defining invalid component groups
      * @param message the error message
      */
    case class InvalidComponentGroups(
      libraryName: LibraryName,
      message: String
    ) extends ValidationError

    /** An error indicating that the library defines a component group extension
      * that extends non-existent component group.
      *
      * @param libraryName the library defining problematic component group
      * extension
      * @param groupReference the group reference to non-existent component
      * group
      */
    case class ComponentGroupExtendsNothing(
      libraryName: LibraryName,
      groupReference: GroupReference
    ) extends ValidationError
  }
}
