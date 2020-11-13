package org.enso.projectmanager.data

/**
  * A class for types with a default value.
  *
  * @tparam A a type parameter
  */
trait Default[A] {

  /**
    * A default value.
    */
  val value: A

}

object Default {

  case class Val[A](override val value: A) extends Default[A]

  def apply[A](implicit default: Default[A]): Default[A] = default

}
