package org.enso.polyglot.data

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TreeTest extends AnyWordSpec with Matchers {

  val tree: Tree.Root[Long] = Tree.Root(
    Vector(
      Tree.Leaf(
        1,
        Vector(
          Tree.Leaf(5, Vector()),
          Tree.Leaf(6, Vector())
        )
      ),
      Tree.Leaf(2, Vector()),
      Tree.Leaf(
        3,
        Vector(
          Tree.Leaf(4, Vector())
        )
      )
    )
  )

  "Tree" should {

    "map" in {
      val expected = Tree.Root(
        Vector(
          Tree.Leaf(
            10,
            Vector(
              Tree.Leaf(50, Vector()),
              Tree.Leaf(60, Vector())
            )
          ),
          Tree.Leaf(20, Vector()),
          Tree.Leaf(
            30,
            Vector(
              Tree.Leaf(40, Vector())
            )
          )
        )
      )
      tree.map(_ * 10) shouldEqual expected
    }

    "filter root" in {
      val expected = Tree.Root(Vector())

      tree.filter(_ > 4) shouldEqual expected
    }

    "filter leaves" in {
      val expected = Tree.Root(
        Vector(
          Tree.Leaf(1, Vector()),
          Tree.Leaf(2, Vector())
        )
      )

      tree.filter(_ < 3) shouldEqual expected
    }

    "fold" in {
      tree.fold(0L)(_ + _) shouldEqual 21L
    }

    "zip" in {
      val tree1 = Tree.Root(
        Vector(
          Tree.Leaf(
            1,
            Vector(Tree.Leaf(10, Vector()))
          ),
          Tree.Leaf(
            2,
            Vector(
              Tree.Leaf(20, Vector()),
              Tree.Leaf(
                21,
                Vector(
                  Tree.Leaf(210, Vector())
                )
              )
            )
          )
        )
      )

      val tree2 = Tree.Root(
        Vector(
          Tree.Leaf(
            2,
            Vector(
              Tree.Leaf(21, Vector()),
              Tree.Leaf(22, Vector())
            )
          ),
          Tree.Leaf(
            3,
            Vector(Tree.Leaf(30, Vector()))
          )
        )
      )

      val expected = Tree.Root(
        Vector(
          Tree.Leaf(These.Here(1), Vector(Tree.Leaf(These.Here(10), Vector()))),
          Tree.Leaf(
            These.Both(2, 2),
            Vector(
              Tree.Leaf(These.Here(20), Vector()),
              Tree.Leaf(
                These.Both(21, 21),
                Vector(
                  Tree.Leaf(These.Here(210), Vector())
                )
              ),
              Tree.Leaf(These.There(22), Vector())
            )
          ),
          Tree.Leaf(
            These.There(3),
            Vector(Tree.Leaf(These.There(30), Vector()))
          )
        )
      )

      Tree.zip(tree1, tree2) shouldEqual expected
    }
  }

}
