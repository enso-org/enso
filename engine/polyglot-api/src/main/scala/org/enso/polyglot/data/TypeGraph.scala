package org.enso.polyglot.data

import com.fasterxml.jackson.annotation.JsonIgnore

import scala.collection.mutable

class TypeGraph(defaultRootType: String) {
  private val parentLinks: mutable.HashMap[String, Set[String]] =
    mutable.HashMap()

  insertDefault(defaultRootType)

  @JsonIgnore
  private def insertDefault(name: String): Unit = {
    parentLinks.update(name, Set())
  }

  @JsonIgnore
  def insert(name: String, parent: String): Unit = {
    parentLinks.updateWith(name) {
      case Some(parents) => Some(parents + parent)
      case None          => Some(Set(parent))
    }
  }

  @JsonIgnore
  def getDirectParents(name: String): Set[String] = {
    parentLinks.getOrElse(name, Set(defaultRootType))
  }

  @JsonIgnore
  def getParents(name: String): Set[String] = {
    var seenNodes: Set[String] = Set()

    val parents = getDirectParents(name)
    parents ++ parents.flatMap(parent => {
      if (!seenNodes.contains(parent)) {
        seenNodes += parent
        getParents(parent)
      } else { Set() }
    })
  }
}
