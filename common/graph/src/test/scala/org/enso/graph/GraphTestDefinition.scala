package org.enso.graph

import org.enso.graph.definition.Macro.{component, field, opaque}
import org.enso.graph.{Graph => PrimGraph}
import shapeless.{::, HNil}

/** This file provides a small graph implementation for testing purposes.
  *
  * It creates a small graph implementation that tests both the various features
  * of the library and of the library's macros. The commented out code has been
  * left _intentionally_ to demonstrate the expansion of the macros here, and to
  * aid in debugging said macros.
  */
object GraphTestDefinition {

  object GraphImpl {

    // ========================================================================
    // === Example Graph Implementation =======================================
    // ========================================================================

    case class Graph() extends PrimGraph

    implicit def components =
      new PrimGraph.Component.List[Graph] {
        type Out = Nodes :: Edges :: HNil
      }

    implicit def nodeFields =
      new PrimGraph.Component.Field.List[Graph, Nodes] {
        type Out =
          Node.Shape :: Node.ParentLink :: Node.Location :: Node.Backref :: HNil
      }

    implicit def edgeFields =
      new PrimGraph.Component.Field.List[Graph, Edges] {
        type Out = Edge.Shape :: HNil
      }

    // ========================================================================
    // === Opaque Storage =====================================================
    // ========================================================================

    @opaque case class Str(opaque: String)
//    sealed case class StringStorage() {
//      val string: mutable.Map[Int, String] = mutable.Map()
//    }

    @opaque case class Backref(opaque: Vector[Int])
//    sealed case class BackrefStorage() {
//      val backref: mutable.Map[Int, Vector[Int]] = mutable.Map()
//    }

    // ========================================================================
    // === Component Definitions ==============================================
    // ========================================================================

    // === Node ===
    @component case class Nodes() { type Node[G <: PrimGraph] }
//    sealed case class Nodes() extends PrimGraph.Component
//    type Node[G <: PrimGraph] = PrimGraph.Component.Ref[G, Nodes]
//    implicit class GraphWithNodes[G <: PrimGraph](
//      graph: PrimGraph.GraphData[G]
//    ) {
//      def addNode()(implicit ev: PrimGraph.HasComponent[G, Nodes]): Node[G] = {
//        graph.addComponent[Nodes]()
//      }
//    }

    // === Edge ===
    @component case class Edges() { type Edge[G <: PrimGraph] }
//    sealed case class Edges() extends PrimGraph.Component
//    type Edge[G <: PrimGraph] = PrimGraph.Component.Ref[G, Edges]
//    implicit class GraphWithEdges[G <: PrimGraph](
//      graph: PrimGraph.GraphData[G]
//    ) {
//      def addEdge()(implicit ev: PrimGraph.HasComponent[G, Edges]): Edge[G] = {
//        graph.addComponent[Edges]()
//      }
//    }

    // ========================================================================
    // === Component Field Definitions ========================================
    // ========================================================================


    object Node {

      @field object Shape {
        type G = PrimGraph
        case class Nul()
        case class App(fn: Edge[G], arg: Edge[G])
        case class Centre(fn: Edge[G])
        case class Name(str: OpaqueData[String, StrStorage], linkEdge: Edge[G])
      }

//      sealed trait Shape extends PrimGraph.Component.Field
//      object Shape {
//        implicit def sized =
//          new Sized[Shape] { type Out = _3 }
//
//        sealed case class Nul() extends Shape
//        sealed case class NulVal[G <: PrimGraph]()
//        object Nul {
//          val index = 0
//          val any =
//            PrimGraph.Component.VariantMatcher[Shape, Nul](index)
//
//          implicit def sized =
//            new Sized[Nul] { type Out = _0 }
//
//          implicit def indexed =
//            new VariantIndexed[Shape, Nul] { val ix = index }
//
//          def unapply[G <: PrimGraph, C <: PrimGraph.Component](
//            arg: PrimGraph.Component.Ref[G, C]
//          )(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Shape]
//          ): Option[NulVal[G]] = {
//            any.unapply(arg).map(t => NulVal())
//          }
//
//          implicit class NulInstance[G <: PrimGraph, C <: PrimGraph.Component](
//            node: PrimGraph.Component.Refined[
//              Shape,
//              App,
//              PrimGraph.Component.Ref[G, C]
//            ]
//          ) {
//            def nul(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): NulVal[G] = NulVal()
//
//            def nul_=(value: NulVal[G])(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Unit = ()
//          }
//        }
//
//        sealed case class App() extends Shape
//        sealed case class AppVal[G <: PrimGraph](fn: Edge[G], arg: Edge[G])
//        object App {
//          val index = 1
//          val any =
//            PrimGraph.Component.VariantMatcher[Shape, App](index)
//          implicit def sized =
//            new Sized[App] { type Out = _2 }
//
//          implicit def indexed =
//            new VariantIndexed[Shape, App] { val ix = index }
//
//          def unapply[G <: PrimGraph, C <: PrimGraph.Component](
//            arg: PrimGraph.Component.Ref[G, C]
//          )(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Shape]
//          ): Option[(Edge[G], Edge[G])] =
//            any.unapply(arg).map(t => scala.Tuple2(t.fn, t.arg))
//
//          implicit class AppInstance[G <: PrimGraph, C <: PrimGraph.Component](
//            node: PrimGraph.Component.Refined[
//              Shape,
//              App,
//              PrimGraph.Component.Ref[G, C]
//            ]
//          ) {
//            def fn(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Edge[G] =
//              PrimGraph.Component.Ref(
//                graph.primUnsafeReadFieldByIndex[C, Shape](
//                  PrimGraph.Component.Refined.unwrap(node).ix,
//                  1
//                )
//              )
//
//            def fn_=(value: Edge[G])(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Unit =
//              graph.primUnsafeWriteFieldByIndex[C, Shape](
//                PrimGraph.Component.Refined.unwrap(node).ix,
//                1,
//                value.ix
//              )
//
//            def arg(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Edge[G] =
//              PrimGraph.Component.Ref(
//                graph.primUnsafeReadFieldByIndex[C, Shape](
//                  PrimGraph.Component.Refined.unwrap(node).ix,
//                  2
//                )
//              )
//
//            def arg_=(value: Edge[G])(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Unit =
//              graph.primUnsafeWriteFieldByIndex[C, Shape](
//                PrimGraph.Component.Refined.unwrap(node).ix,
//                2,
//                value.ix
//              )
//
//            def app(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): AppVal[G] = {
//              AppVal(
//                this.fn,
//                this.arg
//              )
//            }
//
//            def app_=(value: AppVal[G])(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Unit = {
//              this.fn  = value.fn
//              this.arg = value.arg
//            }
//          }
//        }
//
//        // A centre section
//        sealed case class Centre() extends Shape
//        sealed case class CentreVal[G <: PrimGraph](fn: Edge[G])
//        object Centre {
//          val index = 2
//          val any =
//            PrimGraph.Component.VariantMatcher[Shape, Centre](index)
//          implicit def sized =
//            new Sized[Centre] { type Out = _1 }
//          implicit def indexed =
//            new VariantIndexed[Shape, Centre] { val ix = index }
//
//          def unapply[G <: PrimGraph, C <: PrimGraph.Component](
//            arg: PrimGraph.Component.Ref[G, C]
//          )(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Shape]
//          ): Option[scala.Tuple1[Edge[G]]] =
//            any.unapply(arg).map(t => scala.Tuple1(t.fn))
//
//          implicit class CentreInstance[
//            G <: PrimGraph,
//            C <: PrimGraph.Component
//          ](
//            node: PrimGraph.Component.Refined[
//              Shape,
//              Centre,
//              PrimGraph.Component.Ref[G, C]
//            ]
//          ) {
//            def fn(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Edge[G] =
//              PrimGraph.Component.Ref(
//                graph.primUnsafeReadFieldByIndex[C, Shape](
//                  PrimGraph.Component.Refined.unwrap(node).ix,
//                  1
//                )
//              )
//
//            def fn_=(value: Edge[G])(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Unit =
//              graph.primUnsafeWriteFieldByIndex[C, Shape](
//                PrimGraph.Component.Refined.unwrap(node).ix,
//                2,
//                value.ix
//              )
//
//            def centre(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): CentreVal[G] = {
//              CentreVal(
//                this.fn
//                )
//              )
//            }
//
//            def centre_=(value: CentreVal[G])(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Unit = {
//              this.fn = value.fn
//            }
//          }
//        }
//
//        sealed case class Name() extends Shape
//        sealed case class NameVal[G <: PrimGraph](
//          str: String,
//          linkEdge: Edge[G]
//        )
//        object Name {
//          val index = 3
//          val any   = PrimGraph.Component.VariantMatcher[Shape, Name](index)
//          implicit def sized =
//            new Sized[Centre] { type Out = _1 } // Due to one field being opaque
//          implicit def indexed =
//            new VariantIndexed[Shape, Name] { val ix = index }
//
//          def unapply[G <: PrimGraph, C <: PrimGraph.Component](
//            arg: PrimGraph.Component.Ref[G, C]
//          )(
//            implicit graph: PrimGraph.GraphData[G],
//            map: StrStorage,
//            ev: PrimGraph.HasComponentField[G, C, Shape]
//          ): Option[scala.Tuple1[String]] = {
//            any.unapply(arg).map(t => scala.Tuple1(t.str))
//          }
//
//          implicit class NameInstance[
//            G <: PrimGraph,
//            C <: PrimGraph.Component
//          ](
//            node: PrimGraph.Component.Refined[
//              Shape,
//              Name,
//              PrimGraph.Component.Ref[G, C]
//            ]
//          ) {
//            def str(
//              implicit graph: PrimGraph.GraphData[G],
//              map: StrStorage,
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): String = {
//              map.str(PrimGraph.Component.Refined.unwrap(node).ix)
//            }
//
//            def str_=(value: String)(
//              implicit graph: PrimGraph.GraphData[G],
//              map: StrStorage,
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Unit = {
//              map.str(PrimGraph.Component.Refined.unwrap(node).ix) = value
//            }
//
//            def linkEdge(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Edge[G] = {
//              PrimGraph.Component.Ref(
//                graph.primUnsafeReadFieldByIndex[C, Shape](
//                  PrimGraph.Component.Refined.unwrap(node).ix,
//                  1 // as the other field is opaque
//                )
//              )
//            }
//
//            def linkEdge_=(value: Edge[G])(
//              implicit graph: PrimGraph.GraphData[G],
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Unit = {
//              graph.primUnsafeWriteFieldByIndex[C, Shape](
//                PrimGraph.Component.Refined.unwrap(node).ix,
//                1,
//                value.ix
//              )
//            }
//
//            def name(
//              implicit graph: PrimGraph.GraphData[G],
//              map: StrStorage,
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): NameVal[G] = {
//              NameVal(
//                this.str,
//                this.linkEdge
//              )
//            }
//
//            def name_=(value: NameVal[G])(
//              implicit graph: PrimGraph.GraphData[G],
//              map: StrStorage,
//              ev: PrimGraph.HasComponentField[G, C, Shape]
//            ): Unit = {
//              this.str      = value.str
//              this.linkEdge = value.linkEdge
//            }
//          }
//        }
//      }

      @field case class ParentLink[G <: PrimGraph](parent: Edge[G])

      //      sealed case class ParentLink() extends PrimGraph.Component.Field
      //      sealed case class ParentLinkVal[G <: PrimGraph](parent: Edge[G])
      //      object ParentLink {
      //        implicit def sized =
      //          new Sized[ParentLink] { type Out = _1 }
      //
      //        implicit class ParentLinkInstance[
      //          G <: PrimGraph,
      //          C <: PrimGraph.Component
      //        ](
      //          node: PrimGraph.Component.Ref[G, C]
      //        ) {
      //          def parent(
      //            implicit graph: PrimGraph.GraphData[G],
      //            ev: PrimGraph.HasComponentField[G, C, ParentLink]
      //          ): Edge[G] = {
      //            PrimGraph.Component.Ref(
      //              graph.unsafeReadFieldByIndex[C, ParentLink](node.ix, 0)
      //            )
      //          }
      //
      //          def parent_=(value: Edge[G])(
      //            implicit graph: PrimGraph.GraphData[G],
      //            ev: PrimGraph.HasComponentField[G, C, ParentLink]
      //          ): Unit = {
      //            graph.unsafeWriteFieldByIndex[C, ParentLink](node.ix, 0, value.ix)
      //          }
      //
      //          def parentLink(
      //            implicit graph: PrimGraph.GraphData[G],
      //            ev: PrimGraph.HasComponentField[G, C, ParentLink]
      //          ): ParentLinkVal[G] = {
      //            ParentLinkVal(this.parent)
      //          }
      //
      //          def parentLink_=(value: ParentLinkVal[G])(
      //            implicit graph: PrimGraph.GraphData[G],
      //            ev: PrimGraph.HasComponentField[G, C, ParentLink]
      //          ): Unit = {
      //            this.parent = value.parent
      //          }
      //        }
      //
      //        implicit def ParentLink_transInstance[
      //          F <: PrimGraph.Component.Field,
      //          R,
      //          G <: PrimGraph,
      //          C <: PrimGraph.Component
      //        ](
      //          t: PrimGraph.Component.Refined[F, R, PrimGraph.Component.Ref[G, C]]
      //        ): ParentLinkInstance[G, C] = t.wrapped
      //      }

      @field case class Location[G <: PrimGraph](line: Int, column: Int)
//      sealed case class Location() extends PrimGraph.Component.Field;
//      sealed case class LocationVal[G <: PrimGraph](line: Int, column: Int)
//      object Location {
//        implicit def sized =
//          new Sized[Location] { type Out = _2 }
//
//        implicit class LocationInstance[
//          G <: PrimGraph,
//          C <: PrimGraph.Component
//        ](
//          node: PrimGraph.Component.Ref[G, C]
//        ) {
//          def line(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Location]
//          ): Int =
//            graph.unsafeReadField[C, Location](node.ix, 0)
//
//          def line_=(value: Int)(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Location]
//          ): Unit = graph.unsafeWriteFieldByIndex[C, Location](node.ix, 0, value)
//
//          def column(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Location]
//          ): Int =
//            graph.unsafeReadFieldByIndex[C, Location](node.ix, 1)
//
//          def column_=(value: Int)(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Location]
//          ): Unit = graph.unsafeWriteFieldByIndex[C, Location](node.ix, 1, value)
//
//          def location(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Location]
//          ): LocationVal[G] =
//            LocationVal(this.line, this.column)
//
//          def location_=(value: LocationVal[G])(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Location]
//          ): Unit = {
//            this.line   = value.line
//            this.column = value.column
//          }
//        }
//
//        implicit def Location_transInstance[
//          F <: PrimGraph.Component.Field,
//          R,
//          G <: PrimGraph,
//          C <: PrimGraph.Component
//        ](
//          t: PrimGraph.Component.Refined[F, R, PrimGraph.Component.Ref[G, C]]
//        ): LocationInstance[G, C] = t.wrapped
//      }

      @field case class Backref[G <: PrimGraph](
        references: OpaqueData[Vector[Int], BackrefStorage]
      )
//      sealed case class Backref() extends PrimGraph.Component.Field
//      sealed case class BackrefVal[G <: PrimGraph](references: Vector[Int])
//      object Backref {
//        implicit def sized =
//          new Sized[Backref] { type Out = _0 }
//
//        implicit class BackrefInstance[G <: PrimGraph, C <: PrimGraph.Component](
//          node: PrimGraph.Component.Ref[G, C]
//        ) {
//          def references(
//            implicit graph: PrimGraph.GraphData[G],
//            map: BackrefStorage,
//            ev: PrimGraph.HasComponentField[G, C, Backref]
//          ): Vector[Int] = {
//            map.backref(node.ix)
//          }
//
//          def references_=(value: Vector[Int])(
//            implicit graph: PrimGraph.GraphData[G],
//            map: BackrefStorage,
//            ev: PrimGraph.HasComponentField[G, C, Backref]
//          ): Unit = {
//            map.backref(node.ix) = value
//          }
//
//          def backref(
//            implicit graph: PrimGraph.GraphData[G],
//            map: BackrefStorage,
//            ev: PrimGraph.HasComponentField[G, C, Backref]
//          ): BackrefVal[G] = {
//            BackrefVal(this.references)
//          }
//
//          def backref_=(value: BackrefVal[G])(
//            implicit graph: PrimGraph.GraphData[G],
//            map: BackrefStorage,
//            ev: PrimGraph.HasComponentField[G, C, Backref]
//          ): Unit = {
//            this.references = value.references
//          }
//
//          implicit def Location_transInstance[
//            F <: PrimGraph.Component.Field,
//            R,
//            G <: PrimGraph,
//            C <: PrimGraph.Component
//          ](
//            t: PrimGraph.Component.Refined[F, R, PrimGraph.Component.Ref[G, C]]
//          ): BackrefInstance[G, C] = t.wrapped
//        }
//      }
    }

    object Edge {
      @field case class Shape[G <: PrimGraph](source: Node[G], target: Node[G])
//      sealed case class Shape() extends PrimGraph.Component.Field
//      sealed case class ShapeVal[G <: PrimGraph](
//        source: Node[G],
//        target: Node[G]
//      )
//      object Shape {
//        implicit def sized =
//          new Sized[Shape] { type Out = _2 }
//
//        implicit class ShapeInstance[G <: PrimGraph, C <: PrimGraph.Component](
//          node: PrimGraph.Component.Ref[G, C]
//        ) {
//          def source(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Shape]
//          ): Node[G] =
//            PrimGraph.Component.Ref(
//              graph.unsafeReadFieldByIndex[C, Shape](node.ix, 0)
//            )
//
//          def source_=(value: Node[G])(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Shape]
//          ): Unit =
//            graph.unsafeWriteFieldByIndex[C, Shape](node.ix, 0, value.ix)
//
//          def target(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Shape]
//          ): Node[G] =
//            PrimGraph.Component.Ref(
//              graph.unsafeReadFieldByIndex[C, Shape](node.ix, 1)
//            )
//
//          def target_=(value: Node[G])(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Shape]
//          ): Unit =
//            graph.unsafeWriteFieldByIndex[C, Shape](node.ix, 1, value.ix)
//
//          def shape(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Shape]
//          ): ShapeVal[G] = {
//            ShapeVal(this.source, this.target)
//          }
//
//          def shape_=(value: ShapeVal[G])(
//            implicit graph: PrimGraph.GraphData[G],
//            ev: PrimGraph.HasComponentField[G, C, Shape]
//          ): Unit = {
//            this.source = value.source
//            this.target = value.target
//          }
//        }
//
//        implicit def Shape_transInstance[
//          F <: PrimGraph.Component.Field,
//          R,
//          G <: PrimGraph,
//          C <: PrimGraph.Component
//        ](
//          t: PrimGraph.Component.Refined[F, R, PrimGraph.Component.Ref[G, C]]
//        ): ShapeInstance[G, C] = t.wrapped
//      }
    }
  }
}
