package org.enso.polyglot.data

import com.fasterxml.jackson.annotation.JsonIgnore

import scala.collection.mutable

/** A collection that represents subsumption relationships between types.
  *
  * These relationships are useful for dispatch and for collating suggestions
  * for users. All names in this collection should be represented fully
  * qualified.
  *
  * This structure is a graph and may contain loops. The query functions will
  * ensure that an infinite loop doesn't occur in such circumstances.
  *
  * This collection does not implement the Any : Any axiom as that is currently
  * not encoded anywhere in the language.
  *
  * @param defaultRootType the name of the root type of the type subsumption
  *                        hierarchy
  * @param parentLinks the parent links for the types
  */
case class TypeGraph(
  defaultRootType: String,
  parentLinks: mutable.HashMap[String, Set[String]] = mutable.HashMap()
) {
  insertWithoutParent(defaultRootType)

  /** Inserts a type without a parent into the graph.
    *
    * @param name the fully-qualified typename
    */
  @JsonIgnore
  def insertWithoutParent(name: String): Unit = {
    parentLinks.update(name, Set())
  }

  /** Insert a type-parent relationship into the graph.
    *
    * @param typeName the fully-qualified name of the type to set the parent for
    * @param parentName the fully-qualified name of the parent of `typeName`
    */
  @JsonIgnore
  def insert(typeName: String, parentName: String): Unit = {
    parentLinks.updateWith(typeName) {
      case Some(parents) => Some(parents + parentName)
      case None          => Some(Set(parentName))
    }
  }

  /** Get the direct parents of the provided typename.
    *
    * The direct parents of a type are those that it is a child of
    * non-transitively.
    *
    * @param typeName the fully-qualified name of the type to get the direct
    *                 parents for
    * @return the set of direct parents for `typeName`
    */
  @JsonIgnore
  def getDirectParents(typeName: String): Set[String] = {
    parentLinks.getOrElse(typeName, Set(defaultRootType))
  }

  /** Get all of the parents (transitively) of the provided typename.
    *
    * @param typeName the fully-qualified type name for which to get the parents
    * @return all parents of `typeName`
    */
  @JsonIgnore
  def getParents(typeName: String): Set[String] = {
    var seenNodes: Set[String] = Set()

    val parents = getDirectParents(typeName)
    parents ++ parents.flatMap(parent => {
      if (!seenNodes.contains(parent)) {
        seenNodes += parent
        getParents(parent)
      } else { Set() }
    })
  }
}
object TypeGraph {
  def fromJava(rootTypeName: String): TypeGraph =
    new TypeGraph(rootTypeName)
}
