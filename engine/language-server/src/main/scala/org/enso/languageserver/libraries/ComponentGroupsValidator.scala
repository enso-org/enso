package org.enso.languageserver.libraries

import org.enso.editions.LibraryName
import org.enso.pkg.{ComponentGroup, Config, ModuleReference}

import scala.collection.mutable

/** Validate the component groups of provided packages. */
final class ComponentGroupsValidator {

  import ComponentGroupsValidator.ValidationError

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
  ): Iterable[Either[ValidationError, Config]] = {
    val init: Iterable[Right[ValidationError, Config]]           = packages.map(Right(_))
    val modulesMap: mutable.Map[ModuleReference, ComponentGroup] = mutable.Map()

    runValidation(init)(
      validateInvalidComponentGroups,
      validateDuplicateComponentGroups(modulesMap),
      validateComponentGroupExtendsNothing(modulesMap)
    )
  }

  private def validateInvalidComponentGroups
    : Config => Either[ValidationError, Config] = { config =>
    val libraryName = LibraryName(config.namespace, config.name)
    config.componentGroups match {
      case Right(_) =>
        Right(config)
      case Left(e) =>
        Left(
          ValidationError.InvalidComponentGroups(libraryName, e.getMessage())
        )
    }
  }

  private def validateDuplicateComponentGroups(
    modulesMap: mutable.Map[ModuleReference, ComponentGroup]
  ): Config => Either[ValidationError, Config] = { config =>
    val libraryName = LibraryName(config.namespace, config.name)
    config.componentGroups.toOption
      .flatMap { componentGroups =>
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
              Right(config)
            }
          }
          .find(_.isLeft)
      }
      .getOrElse(Right(config))
  }

  private def validateComponentGroupExtendsNothing(
    modulesMap: mutable.Map[ModuleReference, ComponentGroup]
  ): Config => Either[ValidationError, Config] = { config =>
    val libraryName = LibraryName(config.namespace, config.name)
    config.componentGroups.toOption
      .flatMap { componentGroups =>
        componentGroups.extendedGroups
          .map { extendedComponentGroup =>
            if (modulesMap.contains(extendedComponentGroup.module)) {
              Right(config)
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
      }
      .getOrElse(Right(config))
  }

  private def runValidation[A, E](xs: Iterable[Either[E, A]])(
    fs: A => Either[E, A]*
  ): Iterable[Either[E, A]] =
    fs.foldLeft(xs) { (xs, f) =>
      xs.map {
        case Right(a) => f(a)
        case Left(e)  => Left(e)
      }
    }
}

object ComponentGroupsValidator {

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
