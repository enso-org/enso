package org.enso.docs.generator

import scalatags.Text.{all => HTML}
import HTML._

/** An algorithm creating a tree from a list of strings separated by common
  * character into list of list of words, then from that creating a tree with
  * branches from same words on the same depth.
  */
object TreeOfCommonPrefixes {

  /** A single tree node of given `name` containing other nodes as list called
    * `elems`.
    */
  case class Node(name: String, var elems: List[Node]) {

    /** Generates an HTML tree from a tree of nodes, as this method is used to
      * create the page chooser.
      */
    def html(beg: String = ""): Modifier = {
      val newBeg = if (beg.length > 0) beg + "-" + name else name
      if (elems.isEmpty) {
        HTML.li(HTML.a(HTML.href := newBeg)(name))
      } else {
        HTML.li(HTML.`class` := "section")(
          HTML.input(HTML.`type` := "checkbox", HTML.id := newBeg),
          HTML.label(HTML.`for` := newBeg)(
            HTML.a(HTML.href := newBeg)(name)
          ),
          elems.map(x => HTML.ul(x.html(newBeg)))
        )
      }
    }
  }

  /** Groups a list of nodes by it's prefixes.
    */
  def groupNodesByPrefix(le: List[Node], sep: Char): List[Node] =
    groupByPrefix(le.map(_.name), sep)

  /** A function that groups a list of strings by it's prefixes, actually
    * creating the aforementioned tree.
    */
  def groupByPrefix(ls: List[String], sep: Char): List[Node] = {
    var nodes = List[Node]()
    for (string <- ls) {
      if (string.split(sep).length <= 1) {
        nodes = nodes :+ Node(string, List())
      } else {
        val arr      = string.split(sep)
        val filtered = nodes.filter(x => x.name == arr.head)
        if (filtered.nonEmpty && nodes.contains(filtered.head)) {
          nodes.map(n =>
            if (n == filtered.head) {
              n.elems = n.elems :+ Node(arr.tail.mkString(sep.toString), List())
            }
          )
        } else {
          nodes = nodes :+ Node(
            arr.head,
            List(Node(arr.tail.mkString(sep.toString), List()))
          )
        }
      }
    }
    for (node <- nodes) {
      node.elems = groupNodesByPrefix(node.elems, sep)
    }
    nodes
  }
}
