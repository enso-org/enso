package org.enso.data

import scala.reflect.internal.util.WeakHashSet

/** Thread safe pool for objects with 1-1 hashcode-object mapping.
 *
  * Useful for not having lots of duplicate objects in memory.
  *
  * As an example it can be used to pool small strings:
  * `pool.get("Hi"); pool.get("Hi"); pool.get("Hi")`
  * will always return pointer to the first instace "Hi".
  *
  * Once nobody except [[Pool]] holds references to the first
  * "Hi" instance, it will get automatically removed from
  * it on the next garbage collection call.
  *
  * The current usecase is pooling of AST nodes,
  * achieving up to 30% memory savings for nontrivial input programs.
  */
final class Pool[T <: AnyRef] {

  private val astPool = WeakHashSet[T]()

  /** Returns object from pool such that `object == t`, or puts t into pool
    * and returns it, if no such object is found.
    *
    * The asymptotic complexity is almost identical to the of HashSet.get (logN)
    * The function is thread safe and throws an error when t is null
    */
  def get(t: T): T = synchronized(astPool.findEntryOrUpdate(t))

}