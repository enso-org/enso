package org.enso.compiler

import org.enso.compiler.DependentMap.HasOutputType

// TODO [AA] This file contains musings on how to improve some data storage. It
//  will either be used or removed in the next PR.

// TODO [AA] Can we do better?
// TODO [AA] Can we get around HList's type erasure via expanded typeable?
// TODO [AA] Try passing around implicit witnesses of the HList's type.
class DependentMap[K <: HasOutputType] {
  private var data: Map[K, Any] = Map()

  def add(key: K)(value: key.Out): Unit = {
    data = data + (key -> value)
  }

  def get(key: K): Option[key.Out] = {
    data.get(key).map(_.asInstanceOf[key.Out])
  }
}
object DependentMap {
  trait DepMapPairFn[I <: HasOutputType, O <: HasOutputType] {
    def apply(k: I)(v: k.Out): (O, O#Out)
  }

  trait HasOutputType {
    type Out
  }
}

/*

package org.enso.compiler

import org.enso.compiler.DependentMap.{
  ComputeOutputType,
  DepMapPairFn,
  HasOutputType
}
import shapeless.DepFn1

import scala.annotation.unused

// TODO [AA] How could I make this support multiple type fields?
// TODO [AA] Some kind of type projection?
class DependentMap[K <: HasOutputType] {
  private var data: Map[K, Any] = Map()

  def add[O](
    key: K
  )(value: O)(implicit @unused ev: ComputeOutputType.Aux[K, O]): Unit = {
    data = data + (key -> value)
  }

//  def get(key: K): Option[key.Out] = {
//    data.get(key).map(_.asInstanceOf[key.Out])
//  }

//  def map[K2 <: HasOutputType](f: DepMapPairFn[K, K2]): DependentMap[K2] = ???

  // map, foldLeft, filter
}
trait Pass {
  type Config
  type Metadata
}
object DependentMap {
  trait ComputeOutputType[+K <: HasOutputType] {
    type Out
  }
  object ComputeOutputType {
    type Aux[+K <: HasOutputType, X] = ComputeOutputType[K] { type Out = X }
  }

  trait DepMapPairFn[I <: HasOutputType, O <: HasOutputType] {
    def apply(k: I)(v: k.Out): (O, O#Out)
  }

  trait HasOutputType {
    type Out
  }
}

  trait Config
  case object Foo extends Config
  case object Bar extends Config

  trait Pass extends HasOutputType
  case object Baz extends Pass {
    override type Out = Foo.type

    implicit val foo: ComputeOutputType.Aux[Baz.type, Foo.type] =
      new ComputeOutputType[Baz.type] { type Out = Foo.type }
  }
  case object Quux extends Pass {
    override type Out = Bar.type

    implicit val bar: ComputeOutputType.Aux[Quux.type, Bar.type] =
      new ComputeOutputType[Quux.type] { type Out = Bar.type }
  }

  "thing" should {
    "thingy" in {
      val depMap = new DependentMap[Pass]

      import Baz._
      import Quux._

      depMap.add(Baz)(Foo)
      depMap.add(Quux)(Foo)
    }
  }

  def add[K1 <: K, O](
    key: K1
  )(value: O)(implicit @unused ev: ComputeOutputType.Aux[K1, O]): Unit = {
    data = data + (key -> value)
  }


 */
