package org.enso.languageserver.libraries

import org.enso.editions.LibraryName
import org.enso.pkg.{
  ComponentGroups,
  Config,
  ExtendedComponentGroup,
  GroupReference
}

import scala.collection.immutable.ListMap
import scala.collection.{mutable, View}

/** The module allowing to resolve the dependencies between the component groups
  * of different packages.
  */
final class ComponentGroupsResolver {

  /** Run the component groups resolution algorithm.
    *
    * A package can define a new component group or extend an existing one. The
    * resolving algorithm takes the component groups defined by the packages and
    * applies the available extensions.
    *
    * @param packages the list of package configs
    * @return the list of component groups with the dependencies resolved
    */
  def run(packages: Iterable[Config]): Vector[LibraryComponentGroup] = {
    val libraryComponents = packages
      .map { config =>
        LibraryName(config.namespace, config.name) -> config.componentGroups
      }
      .collect { case (libraryName, Right(componentGroups)) =>
        libraryName -> componentGroups
      }
    val libraryComponentsMap =
      ComponentGroupsResolver
        .toMapKeepFirst(libraryComponents)
        .to(ListMap)
    resolveComponentGroups(libraryComponentsMap)
  }

  /** Resolve the component groups. Utility method that takes a list of
    * component groups associated with the library name.
    *
    * @param libraryComponents the associated list of component groups
    * @return the list of component groups with dependencies resolved
    */
  def resolveComponentGroups(
    libraryComponents: Map[LibraryName, ComponentGroups]
  ): Vector[LibraryComponentGroup] = {
    val newLibraryComponentGroups: View[LibraryComponentGroup] =
      libraryComponents.view
        .flatMap { case (libraryName, componentGroups) =>
          componentGroups.newGroups.map(
            LibraryComponentGroup.fromComponentGroup(libraryName, _)
          )
        }
    val newLibraryComponentGroupsMap
      : Map[GroupReference, LibraryComponentGroup] =
      ComponentGroupsResolver
        .groupByKeepFirst(newLibraryComponentGroups) { libraryComponentGroup =>
          GroupReference(
            libraryComponentGroup.library,
            libraryComponentGroup.name
          )
        }

    val extendedComponentGroups: View[ExtendedComponentGroup] =
      libraryComponents.view
        .flatMap { case (_, componentGroups) =>
          componentGroups.extendedGroups
        }
    val extendedComponentGroupsMap
      : Map[GroupReference, Vector[ExtendedComponentGroup]] =
      ComponentGroupsResolver
        .groupByKeepOrder(extendedComponentGroups)(_.group)

    applyExtendedComponentGroups(
      newLibraryComponentGroupsMap,
      extendedComponentGroupsMap
    )
  }

  /** Applies the extended component groups to the existing ones.
    *
    * @param libraryComponentGroups the list of component groups defined by
    * packages
    * @param extendedComponentGroups the list of component groups extending
    * existing ones
    * @return the list of component groups after extended component groups being
    * applied
    */
  private def applyExtendedComponentGroups(
    libraryComponentGroups: Map[GroupReference, LibraryComponentGroup],
    extendedComponentGroups: Map[GroupReference, Seq[ExtendedComponentGroup]]
  ): Vector[LibraryComponentGroup] =
    libraryComponentGroups.map { case (module, libraryComponentGroup) =>
      extendedComponentGroups
        .get(module)
        .fold(libraryComponentGroup) { extendedComponentGroups =>
          extendedComponentGroups
            .foldLeft(libraryComponentGroup)(applyExtendedComponentGroup)
        }
    }.toVector

  /** Applies the extended component group to the target component group.
    *
    * @param libraryComponentGroup the target component group
    * @param extendedComponentGroup the component group to apply
    * @return the resulting component group
    */
  private def applyExtendedComponentGroup(
    libraryComponentGroup: LibraryComponentGroup,
    extendedComponentGroup: ExtendedComponentGroup
  ): LibraryComponentGroup =
    libraryComponentGroup.copy(
      exports = libraryComponentGroup.exports :++
        extendedComponentGroup.exports.map(LibraryComponent.fromComponent)
    )
}

object ComponentGroupsResolver {

  /** Partitions this collection into a map according to some discriminator
    * function, dropping duplicated keys, and preserving the order of elements.
    * E.g. if the discriminator function produces same keys for different
    * values, the resulting map will contain only the first encountered
    * key-value pair for that key.
    *
    * @param xs the source collection
    * @param f the discriminator function
    * @return the grouped collection preserving the order of elements
    */
  private def groupByKeepFirst[K, V](xs: Iterable[V])(f: V => K): Map[K, V] =
    xs
      .foldLeft(mutable.LinkedHashMap.empty[K, V]) { (m, v) =>
        val k = f(v)
        if (m.contains(k)) m
        else m += k -> v
      }
      .toMap

  /** Partitions this collection into a map according to some discriminator
    * function and preserving the order of elements.
    *
    * @param xs the source collection
    * @param f the discriminator function
    * @return the grouped collection that preserves the order of elements
    */
  private def groupByKeepOrder[K, V](
    xs: Iterable[V]
  )(f: V => K): Map[K, Vector[V]] =
    xs
      .foldLeft(mutable.LinkedHashMap.empty[K, Vector[V]]) { (m, v) =>
        m.updateWith(f(v)) {
          case Some(xs) => Some(xs :+ v)
          case None     => Some(Vector(v))
        }
        m
      }
      .toMap

  /** Convert the collection of key-value pairs into a map, dropping duplicated
    * keys, and preserving the order of elements.
    *
    * @param xs the collection of key-value pairs
    * @return the map preserving the order of elements
    */
  private def toMapKeepFirst[K, V](xs: Iterable[(K, V)]): Map[K, V] =
    xs
      .foldLeft(mutable.LinkedHashMap.empty[K, V]) { case (m, (k, v)) =>
        if (m.contains(k)) m
        else m += k -> v
      }
      .toMap
}
