package org.enso.graph

import org.scalatest.{FlatSpec, Matchers}
import shapeless.{::, HNil, Nat}
import shapeless.Nat._

import scala.collection.mutable

class TypeFunctionTest extends FlatSpec with Matchers {

  object HListSumTest {
    implicitly[HListSum.Aux[HNil, _0]]
    implicitly[HListSum.Aux[_1 :: HNil, _1]]
    implicitly[HListSum.Aux[_1 :: _2 :: HNil, _3]]
    implicitly[HListSum.Aux[_1 :: _2 :: _3 :: HNil, _6]]
  }

  object HListTakeUntilTest {
    case class A()
    case class B()
    case class C()
    case class D[G <: Nat]()

    implicitly[HListTakeUntil.Aux[A, HNil, HNil]]
    implicitly[HListTakeUntil.Aux[A, A :: B :: C :: HNil, HNil]]
    implicitly[HListTakeUntil.Aux[B, A :: B :: C :: HNil, A :: HNil]]
    implicitly[HListTakeUntil.Aux[C, A :: B :: C :: HNil, A :: B :: HNil]]
  }

  object MapSizedTest {
    case class A()
    case class B()
    case class C()
    implicit def sizedA = new Sized[A] { type Out = _1 }
    implicit def sizedB = new Sized[B] { type Out = _3 }
    implicit def sizedC = new Sized[C] { type Out = _5 }

    implicitly[MapSized.Aux[HNil, HNil]]
    implicitly[MapSized.Aux[A :: B :: C :: HNil, _1 :: _3 :: _5 :: HNil]]
  }

  object SizeUntilTest {
    case class A()
    case class B()
    case class C()
    implicit def sizedA = new Sized[A] { type Out = _1 }
    implicit def sizedB = new Sized[B] { type Out = _3 }
    implicit def sizedC = new Sized[C] { type Out = _5 }

    implicitly[SizeUntil.Aux[A, HNil, _0]]
    implicitly[SizeUntil.Aux[A, A :: B :: C :: HNil, _0]]
    implicitly[SizeUntil.Aux[B, A :: B :: C :: HNil, _1]]
    implicitly[SizeUntil.Aux[C, A :: B :: C :: HNil, _4]]
  }

  object MapsOfTest {
    case class A()
    case class B()
    case class C()

    implicitly[MapsOf.Aux[HNil, HNil]]
    implicitly[
      MapsOf.Aux[Double :: HNil, mutable.Map[Int, Double] :: HNil]
    ]
    implicitly[MapsOf.Aux[
      String :: Double :: HNil,
      mutable.Map[Int, String] :: mutable.Map[Int, Double] :: HNil
    ]]
  }

  "Generating a HList of Maps" should "work for opaque data" in {
    type MyList = String :: Double :: HNil
    val test = MapsOf[MyList].instance

    test.select[mutable.Map[Int, String]] shouldEqual mutable.Map[Int, String]()
  }

  "A HList of Maps" should "allow selection by Value type" in {
    val mapsOf = MapsOf[String :: Long :: HNil]
    val test = MapsOf[String :: Long :: HNil].instance

    MapsOf.getOpaqueData[Long, mapsOf.Out](test) shouldEqual mutable
      .Map[Int, Long]()
  }

  "The opaque data maps" should "allow insertion and deletion" in {
    val maps = MapsOf[String :: HNil]

    val stringMap =
      MapsOf.getOpaqueData[String, maps.Out](maps.instance)
    val testMap: mutable.Map[Int, String] = mutable.Map()

    stringMap(0) = "TestString"
    testMap(0)   = "TestString"

    MapsOf.getOpaqueData[String, maps.Out](maps.instance) shouldEqual testMap

    stringMap - 0
    testMap - 0

    MapsOf.getOpaqueData[String, maps.Out](maps.instance) shouldEqual testMap
  }
}
