package org.enso.data

import org.enso.data

case class Shifted[+T](off: Int, el: T) {
  def map[S](f: T => S): Shifted[S] =
    Shifted(off, f(el))
}

object Shifted {
  def apply[T](el: T): Shifted[T] = Shifted(0, el)

  case class List1[+T](head: T, tail: List[Shifted[T]]) {
    def map[S](f: T => S): List1[S] =
      List1(f(head), tail.map(_.map(f)))

    def toList(off: Int = 0): List[Shifted[T]] =
      toList1(off).toList

    def toList1(off: Int = 0): data.List1[Shifted[T]] =
      data.List1(Shifted(off, head), tail)

    def +:[B >: T](t: (Int, B)): List1[B] =
      List1(t._2, Shifted(t._1, head) :: tail)

    def +:[B >: T](t: Shifted[B]): List1[B] =
      List1(t.el, Shifted(t.off, head) :: tail)

    def +[B >: T](that: List1[B]): List1[B] =
      List1(head, tail ++ that.toList())

    def +[B >: T](that: List[Shifted[B]]): List1[B] =
      List1(head, tail ++ that)

    def :+[B >: T](that: Shifted[B]): List1[B] =
      List1(head, tail :+ that)
  }

  object List1 {
    def apply[T](head: T): List1[T] = List1(head, Nil)
    implicit def fromTuple[T](t: (T, List[Shifted[T]])): List1[T] =
      List1(t._1, t._2)

    def fromListDropHead[T](lst: List[Shifted[T]]) =
      List1(lst.head.el, lst.tail)
  }

}
