package org.enso.interpreter.bench.fixtures.semantic

object MappyMap {
  sealed trait Map
  case object Tip extends Map
  case class Bin(
    fields: Array[Object]
  ) //size: Object, key: Object, value: Object, left: Map, right: Map)
      extends Map

  object Bin {
    def apply(
      size: Object,
      key: Object,
      value: Object,
      left: Map,
      right: Map
    ): Bin = {
      Bin(Array(size, key, value, left, right))
    }
    object X {
      def unapply(arg: Bin): Option[(Object, Object, Object, Map, Map)] = {
        val fs = arg.fields
        Some(
          (
            fs(0),
            fs(1),
            fs(2),
            fs(3).asInstanceOf[Map],
            fs(4).asInstanceOf[Map]
          )
        )
      }
    }
  }
//  def mk(): MappyMap.type = {
//    println()
//  }

  val x = Bin("Foo", "Bar", "Baz", Tip, Tip)

  def insert(m: Map, key: Object, value: Object): Map =
    m match {
      case Tip => Bin(1.asInstanceOf[Object], key, value, Tip, Tip)
      case Bin.X(s, k, v, l, r) =>
        val keyI = key.asInstanceOf[Int]
        val kI   = k.asInstanceOf[Int]
        if (keyI > kI) {
          balanceR(k, v, l, insert(r, key, value))
        } else if (keyI == kI) {
          Bin(s, k, value, l, r)
        } else {
          balanceL(k, v, insert(l, key, value), r)
        }
    }

  def size(m: Map): Int =
    m match {
      case Tip    => 0
      case Bin.X(s, _, _, _, _) => s.asInstanceOf[Int]
    }

  def empty: Map = Tip

  def balanceL(k: Object, v: Object, left: Map, right: Map): Map = ???

  def balanceR(k: Object, x: Object, l: Map, r: Map): Map = {
    l match {
      case Tip =>
        r match {

          case Tip => Bin(1.asInstanceOf[Object], k, x, Tip, Tip)
          case Bin.X(_, _, _, Tip, Tip) =>
            Bin(2.asInstanceOf[Object], k, x, Tip, r)
          case Bin.X(_, rk, rx, Tip, rr @ Bin.X(_, _, _, _, _)) =>
            Bin(
              3.asInstanceOf[Object],
              rk,
              rx,
              Bin(1.asInstanceOf[Object], k, x, Tip, Tip),
              rr
            )
          case Bin.X(_, rk, rx, Bin.X(_, rlk, rlx, _, _), Tip) =>
            Bin(
              3.asInstanceOf[Object],
              rlk,
              rlx,
              Bin(1.asInstanceOf[Object], k, x, Tip, Tip),
              Bin(1.asInstanceOf[Object], rk, rx, Tip, Tip)
            )
          case Bin.X(
                rsX,
                rk,
                rx,
                rl @ Bin.X(rlsX, rlk, rlx, rll, rlr),
                rr @ Bin.X(rrsX, _, _, _, _)
              ) =>
            val rs: Int  = rsX.asInstanceOf[Int]
            val rls: Int = rlsX.asInstanceOf[Int]
            val rrs: Int = rrsX.asInstanceOf[Int]
            if (rls < ratio * rrs) {
              Bin(
                (1 + rs).asInstanceOf[Object],
                rk,
                rx,
                Bin((1 + rls).asInstanceOf[Object], k, x, Tip, rl),
                rr
              )
            } else {
              Bin(
                (1 + rs).asInstanceOf[Object],
                rlk,
                rlx,
                Bin((1 + size(rll)).asInstanceOf[Object], k, x, Tip, rll),
                Bin((1 + rrs + size(rlr)).asInstanceOf[Object], rk, rx, rlr, rr)
              )
            }
        }
      case Bin.X(lsX, _, _, _, _) =>
        val ls: Int = lsX.asInstanceOf[Int]
        r match {
          case Tip => Bin((1 + ls).asInstanceOf[Object], k, x, l, Tip)
          case Bin.X(rsX, rk, rx, rl, rr) =>
            val rs: Int = rsX.asInstanceOf[Int]
            if (rs > delta * ls) {
              (rl, rr) match {
                case (Bin.X(rlsX, rlk, rlx, rll, rlr), Bin.X(rrsX, _, _, _, _)) =>
                  val rls = rlsX.asInstanceOf[Int]
                  val rrs = rrsX.asInstanceOf[Int]
                  if (rls < ratio * rrs) {
                    Bin(
                      (1 + ls + rs).asInstanceOf[Object],
                      rk,
                      rx,
                      Bin((1 + ls + rls).asInstanceOf[Object], k, x, l, rl),
                      rr
                    )
                  } else {
                    Bin(
                      (1 + ls + rs).asInstanceOf[Object],
                      rlk,
                      rlx,
                      Bin(
                        (1 + ls + size(rll)).asInstanceOf[Object],
                        k,
                        x,
                        l,
                        rll
                      ),
                      Bin(
                        (1 + rrs + size(rlr)).asInstanceOf[Object],
                        rk,
                        rx,
                        rlr,
                        rr
                      )
                    )
                  }
                case _ => ???
              }
            } else {
              Bin((1 + ls + rs).asInstanceOf[Object], k, x, l, r)
            }
        }
    }
  }

  val delta: Int = 3
  val ratio: Int = 2
}
