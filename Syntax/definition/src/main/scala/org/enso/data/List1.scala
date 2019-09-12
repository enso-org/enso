package org.enso

import cats.data.NonEmptyList

package object data {
  type List1[+T] = NonEmptyList[T]
  object List1 {
    def apply[T](el: T, tail: List[T]): List1[T] = new List1(el, tail)
    def apply[T](el: T, tail: T*):      List1[T] = new List1(el, tail.toList)

    def apply[T](list: List[T]): Option[List1[T]] = fromListOption(list)

    def unapply[T](t: List1[T]): Option[(T, List[T])] = Some((t.head, t.tail))

    def fromListOption[T](lst: List[T]): Option[List1[T]] = lst match {
      case Nil     => None
      case t :: ts => Some(List1(t, ts))
    }

    implicit class List1_ops[+T](lst: List1[T]) {
      def mapInit[B >: T](f: T => B): List1[B] =
        if (lst.tail.isEmpty) lst
        else List1(f(lst.head), lst.tail.init.map(f) :+ lst.tail.last)

      def mapLast[B >: T](f: T => B): List1[B] =
        if (lst.tail.isEmpty) List1(f(lst.head), lst.tail)
        else List1(lst.head, lst.tail.init :+ f(lst.tail.last))

      def intersperse[B >: T](t: B): List1[B] =
        List1(lst.head, lst.tail.flatMap(s => List(t, s)))

      def +:[B >: T](that: List[B]): List1[B] = that match {
        case Nil     => lst
        case s :: ss => List1(s, ss ++ lst.toList)
      }

    }
  }
}
