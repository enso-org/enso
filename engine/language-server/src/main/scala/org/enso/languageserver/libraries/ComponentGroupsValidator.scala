package org.enso.languageserver.libraries

import org.enso.editions.LibraryName
import org.enso.pkg.{ComponentGroup, ComponentGroups, Config, ModuleReference}

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
    val modulesMap: mutable.Map[ModuleReference, ComponentGroup] = mutable.Map()
    val init = packages.map { config =>
      val libraryName = LibraryName(config.namespace, config.name)
      libraryName -> validateInvalidComponentGroups(config)
    }

    runValidation(init)(
      validateDuplicateComponentGroups(modulesMap),
      validateComponentGroupExtendsNothing(modulesMap)
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
    val modulesMap: mutable.Map[ModuleReference, ComponentGroup] = mutable.Map()

    runValidation(init)(
      validateDuplicateComponentGroups(modulesMap),
      validateComponentGroupExtendsNothing(modulesMap)
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
    modulesMap: mutable.Map[ModuleReference, ComponentGroup]
  ): Validator = { libraryName => componentGroups =>
    componentGroups.newGroups
      .map { componentGroup =>
        val moduleReference =
          ModuleReference(libraryName, componentGroup.module)
        if (modulesMap.contains(moduleReference)) {
          Left(
            ValidationError
              .DuplicatedComponentGroup(libraryName, moduleReference)
          )
        } else {
          modulesMap += moduleReference -> componentGroup
          Right(componentGroups)
        }
      }
      .find(_.isLeft)
      .getOrElse(Right(componentGroups))
  }

  private def validateComponentGroupExtendsNothing(
    modulesMap: mutable.Map[ModuleReference, ComponentGroup]
  ): Validator = { libraryName => componentGroups =>
    componentGroups.extendedGroups
      .map { extendedComponentGroup =>
        if (modulesMap.contains(extendedComponentGroup.module)) {
          Right(componentGroups)
        } else {
          Left(
            ValidationError.ComponentGroupExtendsNothing(
              libraryName,
              extendedComponentGroup.module
            )
          )
        }
      }
      .find(_.isLeft)
      .getOrElse(Right(componentGroups))
  }

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
      * @param moduleReference the duplicated module reference
      */
    case class DuplicatedComponentGroup(
      libraryName: LibraryName,
      moduleReference: ModuleReference
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
      *                    extension
      * @param moduleReference the module reference to non-existent module
      */
    case class ComponentGroupExtendsNothing(
      libraryName: LibraryName,
      moduleReference: ModuleReference
    ) extends ValidationError
  }
}
