package org.enso.graph

import shapeless.ops.hlist._
import shapeless.ops.{hlist, nat}
import shapeless.{::, HList, HNil, IsDistinctConstraint, Nat}

import scala.annotation.unused
import scala.collection.mutable

// Don't use AnyType here, as it gets boxed sometimes.
import io.estatico.newtype.macros.newtype

// Type-level literals (_0, _1, ...)
import shapeless.nat._

/* TODO [AA, WD] The following are features that we want for this graph:
 *  - `unsafeRemoveComponent` functionality.
 *  - Storage should keep a free-list and re-use space in the underlying buffers
 *    as much as possible in `addComponent`.
 *  - Basic equality testing (that should be overridden as needed).
 *  - An ability to define fields that store complex data such as `String`.
 *  - Add a `Default` typeclass, and ensure that all component fields are
 *    instances of it. Fields should then be initialised using it.
 */

/** This file contains the implementation of an incredibly generic graph.
  *
  * There are a few key notes that should be kept in mind when analysing this
  * solution:
  * - This is a _highly_ generic graph structure, which provides the plumbing
  *   for building your own graph structures.
  * - It only provides an _unsafe_ API. As a result, this graph should never be
  *   used directly by client code. Instead, it should be used to implement a
  *   custom graph structure that provides a safe API.
  */
// ============================================================================
// === HList Generic Utilities ================================================
// ============================================================================

// ================
// === HListSum ===
// ================

/** Sums a [[HList]] that contains only [[Nat]].
  *
  * @tparam L the [[HList]] to sum
  */
trait HListSum[L <: HList] {
  type Out <: Nat
}
object HListSum {
  type Aux[L <: HList, X] = HListSum[L] { type Out = X }

  def apply[L <: HList](implicit ev: HListSum[L]): Aux[L, ev.Out] = ev

  implicit val onNil: HListSum.Aux[HNil, _0] =
    new HListSum[HNil] { type Out = _0 }

  implicit def onCons[H <: Nat, T <: HList, TS <: Nat](
    implicit @unused rest: HListSum.Aux[T, TS],
    all: nat.Sum[H, TS]
  ): HListSum.Aux[H :: T, all.Out] =
    new HListSum[H :: T] { type Out = all.Out }
}

// =======================
// === HListOfNatToVec ===
// =======================

/** Converts an [[HList]] of [[Nat]] to a vector containing those same numbers
  * as integers.
  *
  * @tparam L the [[HList]] to convert
  */
trait HListOfNatToVec[L <: HList] {
  val out: Vector[Int]
}
object HListOfNatToVec {
  implicit def onNil: HListOfNatToVec[HNil] =
    new HListOfNatToVec[HNil] { val out = Vector[Int]() }

  implicit def onCons[Head <: Nat, Tail <: HList](
    implicit
    tail: HListOfNatToVec[Tail],
    head: nat.ToInt[Head]
  ): HListOfNatToVec[Head :: Tail] = new HListOfNatToVec[Head :: Tail] {
    val out = head() +: tail.out
  }
}

// ======================
// === HListTakeUntil ===
// ======================

/** Takes members from an [[HList]] until it reaches a specified member [[T]].
  *
  * The sentinel member [[T]] is not included in the result list. For example,
  * the following code will result in the list `A :: HNil`:
  *
  * {{{
  *   type MyList   = A :: B :: C :: HNil
  *   type Sentinel = B
  *
  *   type HListTakeUntil[Sentinel, MyList]
  * }}}
  *
  * If the sentinel [[T]] is not found in [[Items]], the entire list is
  * returned.
  *
  * @tparam T the sentinel member
  * @tparam Items the list to take members from
  */
trait HListTakeUntil[T, Items <: HList] {
  type Out <: HList
}
object HListTakeUntil extends HListTakeUntilDefaults {
  type Aux[T, Items <: HList, X] = HListTakeUntil[T, Items] { type Out = X }

  def apply[T, Items <: HList](
    implicit ev: HListTakeUntil[T, Items]
  ): Aux[T, Items, ev.Out] = ev

  implicit def onNil[T]: HListTakeUntil.Aux[T, HNil, HNil] =
    new HListTakeUntil[T, HNil] { type Out = HNil }

  implicit def onConsFound[Head, Tail <: HList]
    : HListTakeUntil.Aux[Head, Head :: Tail, HNil] =
    new HListTakeUntil[Head, Head :: Tail] { type Out = HNil }
}

trait HListTakeUntilDefaults {
  implicit def onConsNotFound[T, Head, Tail <: HList, Tail2 <: HList](
    implicit @unused ev1: HListTakeUntil.Aux[T, Tail, Tail2]
  ): HListTakeUntil.Aux[T, Head :: Tail, Head :: Tail2] =
    new HListTakeUntil[T, Head :: Tail] { type Out = Head :: Tail2 }
}

// ============================================================================
// === Graph-specific utilities ===============================================
// ============================================================================

// =============
// === Sized ===
// =============

/** An abstraction for sized objects.
  *
  * Every sized object is aware of the size it occupies (specified as a number
  * of [[Int]]) at compile time.
  *
  * @tparam T the [[Sized]] type
  */
trait Sized[T] {
  type Out <: Nat
}
object Sized {
  type Aux[T, X] = Sized[T] { type Out = X }
  def apply[T](implicit ev: Sized[T]): Aux[T, ev.Out] = ev

  implicit def instance[
    ListOfItems <: HList,
    ListOfSizes <: HList,
    TotalSize <: Nat
  ](
    implicit
    @unused ev1: MapSized.Aux[ListOfItems, ListOfSizes],
    @unused ev2: HListSum.Aux[ListOfSizes, TotalSize]
  ): Sized.Aux[ListOfItems, TotalSize] =
    new Sized[ListOfItems] { type Out = TotalSize }
}

/** A utility for accessing a the Size of a [[Sized]] object as an [[Int]]. */
trait KnownSize[T] extends Sized[T] {
  val asInt: Int
}
object KnownSize {
  implicit def instance[T, Size <: Nat](
    implicit
    @unused ev: Sized.Aux[T, Size],
    sizeEv: nat.ToInt[Size]
  ): KnownSize[T] = new KnownSize[T] { val asInt: Int = sizeEv() }
}

// ================
// === MapSized ===
// ================

/** Extracts the sizes of all types in a list of types. All of these types must
  * be `Sized`.
  *
  * @tparam L the list of elements to extract sizes from
  */
trait MapSized[L <: HList] {
  type Out <: HList
}
object MapSized {
  type Aux[L <: HList, X] = MapSized[L] { type Out = X }

  def apply[L <: HList](implicit ev: MapSized[L]): Aux[L, ev.Out] = ev

  implicit val onNil: MapSized.Aux[HNil, HNil] =
    new MapSized[HNil] { type Out = HNil }

  implicit def onCons[H, T <: HList, TS <: HList, HSize <: Nat](
    implicit @unused rest: MapSized.Aux[T, TS],
    @unused headSize: Sized.Aux[H, HSize]
  ): MapSized.Aux[H :: T, HSize :: TS] =
    new MapSized[H :: T] { type Out = HSize :: TS }
}

// =================
// === SizeUntil ===
// =================

/** Computes the size of the types in a HList up to some sentinel [[Elem]].
  *
  * When summing the sizes in the list, the sentinel element [[Elem]] is not
  * included in the sum.
  *
  * @tparam Elem the type of the element to stop computing at
  * @tparam Items the list of types to compute over
  */
trait SizeUntil[Elem, Items <: HList] {
  type Out <: Nat
  val asInt: Int
}
object SizeUntil {
  type Aux[Elem, Items <: HList, X] = SizeUntil[Elem, Items] { type Out = X }

  def apply[Elem, Items <: HList](
    implicit ev: SizeUntil[Elem, Items]
  ): Aux[Elem, Items, ev.Out] = ev

  implicit def instance[
    Elem,
    Items <: HList,
    PriorElems <: HList,
    PriorFieldSizes <: HList,
    PriorFieldsSize <: Nat
  ](
    implicit
    @unused ev1: HListTakeUntil.Aux[Elem, Items, PriorElems],
    @unused ev2: MapSized.Aux[PriorElems, PriorFieldSizes],
    @unused ev3: HListSum.Aux[PriorFieldSizes, PriorFieldsSize],
    sizeAsInt: nat.ToInt[PriorFieldsSize]
  ): SizeUntil.Aux[Elem, Items, PriorFieldsSize] =
    new SizeUntil[Elem, Items] {
      type Out = PriorFieldsSize
      val asInt = sizeAsInt()
    }
}

/** Produces a [[HList]] of pairs of `(Type, Map[Type])` from a [[HList]] of
  * types.
  *
  * The map it produces is a scala [[mutable.Map]]. Additionally, it has a
  * constraint that no type may appear twice in the input list.
  *
  * @tparam Items the list to start from
  */
trait MapsOf[Items <: HList] {
  type Out <: HList
  val instance: Out
}
object MapsOf {
  type Aux[Items <: HList, X] = MapsOf[Items] { type Out = X }

  def apply[Items <: HList](
    implicit ev: MapsOf[Items]
  ): MapsOf.Aux[Items, ev.Out] = ev

  implicit def onNil: MapsOf.Aux[HNil, HNil] =
    new MapsOf[HNil] {
      type Out = HNil
      val instance = HNil
    }

  implicit def onCons[Head, Tail <: HList](
    implicit ev: MapsOf[Tail],
    @unused distinct: IsDistinctConstraint[Head :: Tail]
  ): MapsOf.Aux[Head :: Tail, mutable.Map[Int, Head] :: ev.Out] =
    new MapsOf[Head :: Tail] {
      type Out = mutable.Map[Int, Head] :: ev.Out
      val instance = mutable.Map[Int, Head]() :: ev.instance
    }

  def getOpaqueData[T, Opaques <: HList](
    list: Opaques
  )(
    implicit ev: Selector[Opaques, mutable.Map[Int, T]]
  ): mutable.Map[Int, T] = {
    list.select[mutable.Map[Int, T]]
  }
}

/** A representation of the index of a variant case within the variant field.
  *
  * A variant node field is one that may take on many different forms under the
  * umbrella of a single field. The prototypical example of this is `Shape`,
  * which can be anything from `Empty` to `TypeDef`. We use the following
  * terminology to describe variant fields:
  * - "The variant" refers to the umbrella type (`Shape` in the example above).
  * - "Branch" revers to the particular variant _element_ which the variant is
  *   taking on at any given time.
  *
  * @tparam F the variant field type
  * @tparam V the variant branch type
  */
trait VariantIndexed[F <: Graph.Component.Field, V <: F] {
  val ix: Int
}

// ============================================================================
// === Graph ==================================================================
// ============================================================================

/** A generic graph implementation.
  *
  * The type [[Graph]] should not be used directly by programs that use this
  * library. Instead, it should be used to implement custom graph instances by
  * extending the [[Graph]] trait.
  */
trait Graph
object Graph {

  // ==========================
  // === Smart Constructors ===
  // ==========================

  def apply[G <: Graph: GraphInfo](): GraphData[G] = new GraphData[G]()

  // =================
  // === Component ===
  // =================

  /** A graph component is a type of thing that can be stored in the graph.
    *
    * Components can be arbitrary, such as nodes, edges, groups, and so on. They
    * are defined entirely by the users, and this is done by extending the
    * [[Component]] trait. Please see the tests for examples of how this can be
    * done.
    */
  trait Component
  object Component {

    // === Ref ===

    /** A generic reference to a graph component.
      *
      * For example, the `Node` type could be defined as follows, where `Nodes`
      * is the appropriate component in the graph.
      *
      * {{{
      *   type Node = Ref[MyGraph, Nodes]
      * }}}
      */
    @newtype
    final case class Ref[G <: Graph, C <: Component](ix: Int)

    /** Type refinement for component references.
      *
      * Type refinement is used to add additional information to a [[Component]]
      * to encode properties into the graph structure. This information can be
      * type information, but can also be used to tag graph components with
      * arbitrary properties.
      *
      * For example, a node with information that its shape is `App` can be
      * encoded having the following type `Refined[Shape, App, Node]`.
      */
    @newtype
    final case class Refined[F <: Component.Field, Spec, T](wrapped: T)
    object Refined {
      implicit def unwrap[F <: Component.Field, S <: F, T](
        t: Refined[F, S, T]
      ): T = { t.wrapped }

      def wrap[F <: Component.Field, S <: F, T](t: T): Refined[F, S, T] =
        Refined(t)
    }

    // === List ===

    /** Defines the set of components in a graph by assigning a type to the
      * `Out` parameter when implementing the trait.
      *
      * This is used to encode the graph structure at the type level. An example
      * is as follows:
      *
      * {{{
      *   implicit def components = new Component.List[MyGraph] {
      *     type Out = Nodes :: Edges :: HNil
      *   }
      * }}}
      *
      * @tparam G the graph for which the components are defined.
      */
    // TODO [AA] Use a type level map/filter to make this more robust. The props
    //  should be checked at compile time even if a thing isn't used.
    trait List[G <: Graph] {
      type Out <: HList
    }
    object List {
      type Aux[G <: Graph, X] = List[G] { type Out = X }
    }

    // === Field ===

    /** A field is a portion of a [[Component]].
      *
      * They are used to attribute data to components, and store data within a
      * component.
      *
      * An example would be a component `Node`, that consists of fields such as
      * `ParentLink` and `Shape`.
      */
    trait Field
    object Field {

      /** Defines the set of fields for a given kind of component.
        *
        * An example is as follows:
        *
        * {{{
        *   implicit def nodeFields = new Component.Field.List[MyGraph, Nodes] {
        *     type Out = Node.Shape :: Node.ParentLink :: HNil
        *   }
        * }}}
        *
        * @tparam G the graph to which the component type [[C]] belongs
        * @tparam C the component type to which the fields in the list belong
        */
      // TODO [AA] Use a type level map/filter to make this more robust. Props
      //  should be checked at compile time even if a thing isn't used.
      trait List[G <: Graph, C <: Component] { type Out <: HList }
      object List {
        type Aux[G <: Graph, C <: Component, X] = List[G, C] { type Out = X }
      }
    }

    // === Storage ===

    /** Specialized storage for component data.
      *
      * We intentionally do not use [[scala.collection.mutable.ArrayBuffer]] as
      * it cannot be specialised for primitive types. [[Array]], on the other
      * hand, can be.
      */
    final class Storage(@unused elemSize: Int) {
      var length: Int       = 0
      var array: Array[Int] = new Array[Int](length)

      // TODO: Assert that elem size = elemSize
      def push(elem: Array[Int]): Unit = {
        this.array = this.array ++ elem
        this.length += 1
      }
    }

    // === VariantMatcher ===

    /** A utility for generating unapply and match methods for a [[Component]]
      * that contains a sum type.
      *
      * It is used internally by the [[org.enso.graph.definition.Macro.field]]
      * macro to autogenerate matchers for variant fields.
      */
    case class VariantMatcher[T <: Component.Field, V](ix: Int) {
      def unapply[G <: Graph, C <: Component](
        arg: Component.Ref[G, C]
      )(
        implicit graph: GraphData[G],
        ev: HasComponentField[G, C, T]
      ): Option[Component.Refined[T, V, Component.Ref[G, C]]] = {
        val variantIndexByteOffset = 0
        if (graph.unsafeReadFieldByIndex[C, T](arg.ix, variantIndexByteOffset) == ix)
          Some(Component.Refined[T, V, Component.Ref[G, C]](arg))
        else None
      }
    }
  }

  // =================
  // === GraphData ===
  // =================

  /** [[GraphData]] is the underlying storage representation used by the
    * [[Graph]].
    *
    * It contains the raw data for all the graph components and their fields.
    *
    * @param info information about the graph's underlying structure
    * @tparam G the graph type that the data is for
    */
  final class GraphData[G <: Graph]()(implicit val info: GraphInfo[G]) {
    var components: Array[Component.Storage] =
      this.componentSizes.map(size => new Component.Storage(size)).toArray

    /** Gets a reference to the graph component at the specified index.
      *
      * No bounds checking is done on the provided index.
      *
      * @param index the index of the component to access
      * @param ev evidence that the graph [[G]] has a component [[C]]
      * @tparam C the type of the component to access
      * @return a reference to the component at `index`
      */
    def componentRefFromIndex[C <: Component](index: Int)(
      implicit @unused ev: HasComponent[G, C]
    ): Graph.Component.Ref[G, C] = {
      Graph.Component.Ref(index)
    }

    /** Sets a variant node field to the specified case.
      *
      * @param component the component to set the field in
      * @param info evidence that the component [[C]] has field [[F]] in [[G]]
      * @param indexed evidence that [[V]] is an indexed field in the variant
      *                [[F]]
      * @tparam C the component type
      * @tparam F the field type
      * @tparam V the variant branch type
      */
    def unsafeSetVariantCase[C <: Component, F <: Component.Field, V <: F](
      component: Component.Ref[G, C]
    )(
      implicit info: HasComponentField[G, C, F],
      indexed: VariantIndexed[F, V]
    ): Unit = {
      unsafeWriteField[C, F](component, indexed.ix)
    }

    /** Reads the data for a specified field [[F]] in a component [[C]] from the
      * graph for the component instance [[component]].
      *
      * @param component the component instance to read from
      * @param info evidence that component [[C]] has field [[F]] in [[G]]
      * @tparam C the component type
      * @tparam F the field type
      * @return the raw field data from [[component]] specified by [[F]]
      */
    def unsafeGetFieldData[C <: Component, F <: Component.Field](
      component: Component.Ref[G, C]
    )(implicit info: HasComponentField[G, C, F]): (Array[Int], Int) = {
      unsafeGetFieldDataByIndex[C, F](component.ix, info.fieldOffset)
    }

    /** Reads the data for a specified field [[F]] in a component [[C]] from the
      * graph for the component instance represented by [[componentIx]].
      *
      * No bounds checking is performed on any of the provided indices.
      *
      * @param componentIx the index representing the component to read from
      * @param fieldIx the index of the field [[F]] in the component [[C]]
      * @param info evidence that component [[C]] has field [[F]] in [[G]]
      * @tparam C the component type
      * @tparam F the field type
      * @return the raw field data from [[component]] specified by [[F]]
      */
    def unsafeGetFieldDataByIndex[C <: Component, F <: Component.Field](
      componentIx: Int,
      fieldIx: Int
    )(implicit info: HasComponentField[G, C, F]): (Array[Int], Int) = {
      val arr = components(info.componentIndex).array
      val idx = info.componentSize * componentIx + info.fieldOffset + fieldIx
      (arr, idx)
    }

    /** Reads the spcified field [[F]] from [[component]].
      *
      * @param component the component instance to read from
      * @param ev evidence that component [[C]] has field [[F]] in [[G]]
      * @tparam C the component type
      * @tparam F the field type
      * @return the value of field [[F]] in [[component]]
      */
    def unsafeReadField[C <: Component, F <: Component.Field](
      component: Component.Ref[G, C]
    )(implicit ev: HasComponentField[G, C, F]): Int = {
      unsafeReadFieldByIndex[C, F](component.ix, ev.fieldOffset)
    }

    /** Reads the spcified field [[F]] from the instance of [[C]] at
      * [[componentIx]].
      *
      * No bounds checking is performed on any of the provided indices.
      *
      * @param componentIx the index representing the component [[C]]
      * @param fieldIx the index of the field [[F]] in the component [[C]]
      * @param ev evidence that component [[C]] has field [[F]] in [[G]]
      * @tparam C the component type
      * @tparam F the field type
      * @return the value of field [[F]] in [[component]]
      */
    def unsafeReadFieldByIndex[C <: Component, F <: Component.Field](
      componentIx: Int,
      fieldIx: Int
    )(implicit ev: HasComponentField[G, C, F]): Int = {
      val (arr, idx) = unsafeGetFieldDataByIndex(componentIx, fieldIx)
      arr(idx)
    }

    /** Writes the value of field [[F]] in [[component]] to be [[value]].
      *
      * @param component the instance of [[C]] to write to
      * @param value the value to write to the field [[F]]
      * @param ev evidence that component [[C]] has field [[F]] in [[G]]
      * @tparam C the component type
      * @tparam F the field type
      */
    def unsafeWriteField[C <: Component, F <: Component.Field](
      component: Component.Ref[G, C],
      value: Int
    )(
      implicit ev: HasComponentField[G, C, F]
    ): Unit = {
      unsafeWriteFieldByIndex[C, F](component.ix, ev.fieldOffset, value)
    }

    /** Sets the field at [[fieldIx]] in the instance of [[C]] represented by
      * [[componentIx]] to have the value [[value]].
      *
      * No bounds checking is performed on any of the provided indices.
      *
      * @param componentIx the index representing an instance of [[C]]
      * @param fieldIx the index of the field [[F]] in [[C]]
      * @param value the value to set [[F]] to
      * @param ev evidence that component [[C]] has field [[F]] in [[G]]
      * @tparam C the component type
      * @tparam F the field type
      */
    def unsafeWriteFieldByIndex[C <: Component, F <: Component.Field](
      componentIx: Int,
      fieldIx: Int,
      value: Int
    )(implicit ev: HasComponentField[G, C, F]): Unit = {
      val (arr, idx) = unsafeGetFieldDataByIndex(componentIx, fieldIx)
      arr(idx) = value
    }

    /** Adds a new instance of component [[C]] to the graph.
      *
      * @param info evidence that the graph [[G]] has component [[C]]
      * @tparam C the component type
      * @return a reference to the new component [[C]]
      */
    def addComponent[C <: Component]()(
      implicit info: HasComponent[G, C]
    ): Component.Ref[G, C] = {
      val compClsIx = info.componentIndex
      val compIx    = components(compClsIx).length
      val data      = new Array[Int](info.componentSize)
      components(compClsIx).push(data)
      Component.Ref(compIx)
    }
  }
  object GraphData {

    /** Gets the [[GraphInfo]] from the [[GraphData]] instance.
      *
      * @param g the graph data
      * @tparam G the graph type
      * @return the graph info for [[G]]
      */
    implicit def getInfo[G <: Graph](g: GraphData[G]): GraphInfo[G] = g.info
  }

  // ====================
  // === TypeFamilies ===
  // ====================

  // === GraphInfo ===

  /** Information about the number and sizes of components stored in the graph.
    *
    * @tparam G the graph for which this metadata exists
    */
  trait GraphInfo[G <: Graph] {
    val componentCount: Int
    val componentSizes: Vector[Int]
  }
  object GraphInfo {
    implicit def instance[
      G <: Graph,
      ComponentList <: HList,
      ComponentSizeList >: HList,
      ComponentListLength <: Nat
    ](
      implicit
      @unused ev1: Component.List.Aux[G, ComponentList],
      @unused ev2: hlist.Length.Aux[ComponentList, ComponentListLength],
      componentSizesEv: ComponentListToSizes[G, ComponentList],
      len: nat.ToInt[ComponentListLength]
    ): GraphInfo[G] = new GraphInfo[G] {
      val componentCount = len()
      val componentSizes = componentSizesEv.sizes
    }
  }

  // === HasComponent ===

  /** Encodes that a given graph [[G]] has a component with given type [[C]].
    *
    * @tparam G the graph type
    * @tparam C the component type
    */
  trait HasComponent[G <: Graph, C <: Component] {
    val componentIndex: Int
    val componentSize: Int
  }
  object HasComponent {
    implicit def instance[
      G <: Graph,
      C <: Component,
      ComponentList <: HList,
      PrevComponentList <: HList,
      ComponentIndex <: Nat,
      FieldList <: HList
    ](
      implicit
      @unused ev1: Component.List.Aux[G, ComponentList],
      @unused ev2: Component.Field.List.Aux[G, C, FieldList],
      @unused ev3: HListTakeUntil.Aux[C, ComponentList, PrevComponentList],
      @unused ev4: hlist.Length.Aux[PrevComponentList, ComponentIndex],
      componentIndexEv: nat.ToInt[ComponentIndex],
      componentSizeEv: KnownSize[FieldList],
      @unused listContainsComponent: Selector[ComponentList, C]
    ): HasComponent[G, C] = new HasComponent[G, C] {
      val componentIndex = componentIndexEv()
      val componentSize  = componentSizeEv.asInt
    }
  }

  // === HasComponentField ===

  /** Encodes that a graph [[G]] has field [[F]] in component [[C]].
    *
    * @tparam G the graph type
    * @tparam C the component type in [[G]]
    * @tparam F the field type in [[C]]
    */
  trait HasComponentField[G <: Graph, C <: Component, F <: Component.Field] {
    val componentIndex: Int
    val componentSize: Int
    val fieldOffset: Int
  }
  object HasComponentField {
    implicit def instance[
      G <: Graph,
      C <: Component,
      F <: Component.Field,
      FieldList <: HList
    ](
      implicit
      @unused ev1: Component.Field.List.Aux[G, C, FieldList],
      evx: HasComponent[G, C],
      fieldOffsetEv: SizeUntil[F, FieldList],
      @unused containsField: Selector[FieldList, F]
    ): HasComponentField[G, C, F] = new HasComponentField[G, C, F] {
      val componentIndex = evx.componentIndex
      val componentSize  = evx.componentSize
      val fieldOffset    = fieldOffsetEv.asInt
    }
  }

  // === ComponentListToSizes ===

  /** Obtains the sizes of all the components from the graph's list of
    * components.
    *
    * @tparam G the graph
    * @tparam ComponentList the list of components
    */
  trait ComponentListToSizes[G <: Graph, ComponentList <: HList] {
    val sizes: Vector[Int]
  }
  object ComponentListToSizes {
    implicit def onNil[G <: Graph]: ComponentListToSizes[G, HNil] =
      new ComponentListToSizes[G, HNil] { val sizes = Vector[Int]() }

    implicit def onCons[G <: Graph, C <: Component, Tail <: HList](
      implicit
      tail: ComponentListToSizes[G, Tail],
      info: HasComponent[G, C]
    ): ComponentListToSizes[G, C :: Tail] =
      new ComponentListToSizes[G, C :: Tail] {
        val sizes = info.componentSize +: tail.sizes
      }
  }

  /** Allows casting between variant cases without actually mutating the
    * underlying structure.
    *
    * This is a very unsafe operation and should be used with care.
    *
    * @param component the component to cast the variant field in
    * @param ev evidence that component [[C]] has field [[G]] in graph [[G]]
    * @tparam G the graph type
    * @tparam C the component type
    * @tparam F the field type
    */
  implicit class VariantCast[
    G <: Graph,
    C <: Component,
    F <: Component.Field
  ](val component: Component.Ref[G, C])(
    implicit ev: HasComponentField[G, C, F],
    graph: GraphData[G]
  ) {

    /** Checks if [[component]] is in the variant case denoted by the type
      * [[V]].
      *
      * @param variantIndexed information that [[F]] is indeed a variant, with
      *                       [[V]] as a valid case
      * @tparam V the type of the variant case in question
      * @return `true` if [[component]] is of the form denoted by [[V]], `false`
      *         otherwise
      */
    def is[V <: F](
      implicit variantIndexed: VariantIndexed[F, V]
    ): Boolean = {
      graph.unsafeReadField[C, F](component) == variantIndexed.ix
    }

    /** Casts the variant field [[F]] to behave as the variant branch [[V]].
      *
      * It should be noted that this is purely a superficial cast, and does not
      * affect the underlying graph. This means that [[C]] will still pattern
      * match as if it was its original variant branch.
      *
      * @param variantIndexed information that [[F]] is indeed a variant, with
      *                       [[V]] as a valid case
      * @tparam V the type of the variant case in question
      * @return the component [[component]] refined to be the variant branch
      *         [[V]]
      */
    def unsafeAs[V <: F](
      implicit @unused variantIndexed: VariantIndexed[F, V]
    ): Component.Refined[F, V, Component.Ref[G, C]] = {
      Component.Refined[F, V, Component.Ref[G, C]](component)
    }

    /** Performs a checked cast of [[component]] to the variant state denoted
      * by [[V]].
      *
      * @param variantIndexed information that [[F]] is indeed a variant, with
      *                       [[V]] as a valid case
      * @tparam V the type of the variant case in question
      * @return [[Some]] if [[component]] is a [[V]], otherwise [[None]]
      */
    def as[V <: F](
      implicit variantIndexed: VariantIndexed[F, V]
    ): Option[Component.Refined[F, V, Component.Ref[G, C]]] = {
      if (is[V]) {
        Some(unsafeAs[V])
      } else {
        None
      }
    }

    // TODO [AA] Add a utility to automatically refine variant branches
  }
}
