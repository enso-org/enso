package org.enso.polyglot.data

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable.Builder

class TreeTest extends AnyWordSpec with Matchers {

  val tree: Tree.Root[Long] = Tree.Root(
    Vector(
      Tree.Node(
        1,
        Vector(
          Tree.Node(5, Vector()),
          Tree.Node(6, Vector())
        )
      ),
      Tree.Node(2, Vector()),
      Tree.Node(
        3,
        Vector(
          Tree.Node(4, Vector())
        )
      )
    )
  )

  "Tree" should {

    "map" in {
      val expected = Tree.Root(
        Vector(
          Tree.Node(
            10,
            Vector(
              Tree.Node(50, Vector()),
              Tree.Node(60, Vector())
            )
          ),
          Tree.Node(20, Vector()),
          Tree.Node(
            30,
            Vector(
              Tree.Node(40, Vector())
            )
          )
        )
      )
      tree.map(_ * 10) shouldEqual expected
    }

    "filter root" in {
      val expected = Tree.Root(
        Vector(
          Tree.Node(
            1,
            Vector(
              Tree.Node(5, Vector()),
              Tree.Node(6, Vector())
            )
          )
        )
      )

      tree.filter(_ > 4) shouldEqual expected
    }

    "filter nodes" in {
      val expected = Tree.Root(
        Vector(
          Tree.Node(1, Vector()),
          Tree.Node(2, Vector())
        )
      )

      tree.filter(_ < 3) shouldEqual expected
    }

    "fold" in {
      tree.fold(0L)(_ + _) shouldEqual 21L
    }

    "zip roots" in {
      val tree1 = Tree.Root(
        Vector(
          Tree.Node(
            1,
            Vector(Tree.Node(10, Vector()))
          ),
          Tree.Node(
            2,
            Vector(
              Tree.Node(20, Vector()),
              Tree.Node(
                21,
                Vector(
                  Tree.Node(210, Vector())
                )
              )
            )
          )
        )
      )

      val tree2 = Tree.Root(
        Vector(
          Tree.Node(
            2,
            Vector(
              Tree.Node(21, Vector()),
              Tree.Node(22, Vector())
            )
          ),
          Tree.Node(
            3,
            Vector(Tree.Node(30, Vector()))
          )
        )
      )

      val expected = Tree.Root(
        Vector(
          Tree.Node(These.Here(1), Vector(Tree.Node(These.Here(10), Vector()))),
          Tree.Node(
            These.Both(2, 2),
            Vector(
              Tree.Node(These.Here(20), Vector()),
              Tree.Node(
                These.Both(21, 21),
                Vector(
                  Tree.Node(These.Here(210), Vector())
                )
              ),
              Tree.Node(These.There(22), Vector())
            )
          ),
          Tree.Node(
            These.There(3),
            Vector(Tree.Node(These.There(30), Vector()))
          )
        )
      )

      Tree.zip(tree1, tree2) shouldEqual expected
    }
  }

  "zip nodes" in {
    val tree1 =
      Tree.Node(
        2,
        Vector(
          Tree.Node(20, Vector()),
          Tree.Node(
            21,
            Vector(
              Tree.Node(210, Vector())
            )
          )
        )
      )

    val tree2 =
      Tree.Node(
        2,
        Vector(
          Tree.Node(21, Vector()),
          Tree.Node(22, Vector())
        )
      )

    val expected = Tree.Root(
      Vector(
        Tree.Node(
          These.Both(2, 2),
          Vector(
            Tree.Node(These.Here(20), Vector()),
            Tree.Node(
              These.Both(21, 21),
              Vector(
                Tree.Node(These.Here(210), Vector())
              )
            ),
            Tree.Node(These.There(22), Vector())
          )
        )
      )
    )

    Tree.zip(tree1, tree2) shouldEqual expected
  }

  "building the tree with mutable builder" in {
    val b: Builder[Tree.Node[String], Vector[
      Tree.Node[String]
    ]] = Vector.newBuilder
    b += Tree.Node("ahoj", Vector())
    b += Tree.Node("cześć", Vector())
    b += Tree.Node("hi", Vector())
    b += Tree.Node("приве́т", Vector())
    b += Tree.Node("ciao", Vector())
    val v = b.result()
    val t = Tree.Root(v)

    val expected = Tree.Root(
      Vector(
        Tree.Node("ahoj", Vector()),
        Tree.Node("hi", Vector()),
        Tree.Node("ciao", Vector())
      )
    )

    t.filter(_.forall(ch => 'a' <= ch && ch <= 'z')) shouldEqual expected
  }
}
