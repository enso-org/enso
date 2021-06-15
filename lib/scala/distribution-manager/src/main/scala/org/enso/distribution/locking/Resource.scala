package org.enso.distribution.locking

/** Represents a resource that can be locked. */
trait Resource {

  /** Name of the resource.
    *
    * Must be a valid filename part.
    */
  def name: String

  /** A message that is displayed by default if the lock on that resource cannot
    * be acquired immediately.
    */
  def waitMessage: String
}
