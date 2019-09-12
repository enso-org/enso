package org.enso.data

import org.enso.data.VectorIntMap.Index

/** Read only zipper-like map.
  *
  * Sparse, sorted, vector based map
  * with O(1) access, when accessing adjacent keys
  * with O(N) access, when accessing random keys
  * this is achieved by remembering the index of
  * last accessed key in class[[Index]]
  */
final class VectorIntMap[V](values: Seq[(Int, V)] = Seq()) {
  private val vector = values.toVector.sortBy(_._1)

  def get(key: Int, i: Index): Option[V] = {
    def _key  = vector(i.index)._1 + i.offset
    def value = vector(i.index)._2

    while (i.index < vector.length && _key <= key) {
      if (_key == key)
        return Some(value)
      i.index += 1
    }
    i.index -= 1
    while (i.index >= 0 && _key >= key) {
      if (_key == key)
        return Some(value)
      i.index -= 1
    }
    i.index += 1
    None
  }
}

object VectorIntMap {
  class Index(var index: Int, val offset: Int = 0)
}
