package org.enso

import scala.annotation.unused
import scala.reflect.macros.blackbox.Context

class Logger {
  import Logger._

  var nesting = 0

  def log(s: String): Unit =
    macro funcRedirect

  def warn(s: String): Unit =
    macro funcRedirect

  def err(s: String): Unit =
    macro funcRedirect

  def group[T](msg: String)(body: => T): T =
    macro groupRedirect[T]

  def trace[T](body: => T): T =
    macro targetRedirect[T]

  def trace_[T](body: => T): T =
    macro targetRedirect_[T]

  def _log(msg: String): Unit =
    println("|  " * nesting + msg)

  def _warn(msg: String): Unit =
    println("|  " * nesting + Console.YELLOW + msg + Console.RESET)

  def _err(msg: String): Unit =
    println("|  " * nesting + Console.RED + msg + Console.RESET)

  def _group[T](msg: String)(body: => T): T = {
    _log(msg)
    beginGroup()
    val out = body
    endGroup()
    out
  }

  def _trace[T](msg: String)(body: => T): T = {
    _log(msg)
    beginGroup()
    val out = body
    endGroup()
    out
  }

  def _trace_[T](msg: String)(body: => T): T = {
    _log(msg)
    beginGroup()
    val out = body
    endGroup()
    out
  }

  def beginGroup(): Unit =
    nesting += 1

  def endGroup(): Unit =
    nesting -= 1

}

object Logger {
  def groupRedirect[R: c.WeakTypeTag](
    c: Context
  )(@unused msg: c.Tree)(body: c.Tree): c.Expr[R] = {
    import c.universe._
    val target = c.macroApplication match {
      case Apply(Apply(TypeApply(Select(base, name), typ), msg2), body2) =>
        val newName = TermName("_" + name.toString)
        Apply(Apply(TypeApply(Select(base, newName), typ), msg2), body2)
      case _ => throw new Error("Unsupported shape")
    }
    if (checkEnabled(c)) c.Expr(q"$target") else c.Expr(q"$body")
  }

  def targetRedirect[R: c.WeakTypeTag](c: Context)(body: c.Tree): c.Expr[R] = {
    import c.universe._
    val target = c.macroApplication match {
      case Apply(TypeApply(Select(base, name), typ), body2) =>
        val newName      = TermName("_" + name.toString)
        val owner        = c.internal.enclosingOwner.asMethod
        val owner2       = owner.owner
        val parentObject = !owner2.isStatic
        val oname =
          if (parentObject) owner2.name.toString + "." + owner.name.toString
          else owner.name.toString
        val ownerName = Literal(Constant(oname))
        owner.paramLists match {
          case lst :: _ =>
            val lst2 = lst.map(x => q"$x")
            val msg =
              if (lst2.isEmpty) List(q"$ownerName")
              else List(q"$ownerName + $lst2.toString().drop(4)")
            Apply(Apply(TypeApply(Select(base, newName), typ), msg), body2)
          case _ => throw new Error("Unsupported shape")
        }
      case _ => throw new Error("Unsupported shape")
    }
    if (checkEnabled(c)) c.Expr(q"$target") else c.Expr(q"$body")
  }

  def targetRedirect_[R: c.WeakTypeTag](c: Context)(body: c.Tree): c.Expr[R] = {
    import c.universe._
    val target = c.macroApplication match {
      case Apply(TypeApply(Select(base, name), typ), body2) =>
        val newName      = TermName("_" + name.toString)
        val owner        = c.internal.enclosingOwner.asMethod
        val owner2       = owner.owner
        val parentObject = !owner2.isStatic
        val oname =
          if (parentObject) owner2.name.toString + "." + owner.name.toString
          else owner.name.toString
        val ownerName = Literal(Constant(oname))
        owner.paramLists match {
          case _ :: _ =>
            val msg = List(q"$ownerName")
            Apply(Apply(TypeApply(Select(base, newName), typ), msg), body2)
          case _ => throw new Error("Unsupported shape")
        }
      case _ => throw new Error("Unsupported shape")
    }
    if (checkEnabled(c)) c.Expr(q"$target") else c.Expr(q"$body")
  }

  def funcRedirect(c: Context)(@unused s: c.Tree): c.Expr[Unit] = {
    import c.universe._
    val target = c.macroApplication match {
      case Apply(Select(base, name), args) =>
        val newName = TermName("_" + name.toString)
        Apply(Select(base, newName), args)
      case _ => throw new Error("Unsupported shape")
    }
    if (checkEnabled(c)) c.Expr(q"$target") else c.Expr(q"{}")
  }

  def checkEnabled(c: Context): Boolean = {
    val optPfx  = "logging"
    val opts    = c.settings.filter(_.matches(s"(\\+|\\-)$optPfx.*"))
    val owner   = c.internal.enclosingOwner.fullName
    var enabled = true
    opts.foreach { opt =>
      val sign   = opt.head
      val body   = opt.tail.drop(optPfx.length)
      val status = sign == '+'
      val applies =
        if (body == "") true
        else {
          val pathPfx = body.head
          val path    = body.tail
          pathPfx == '@' && owner.startsWith(path)
        }
      if (applies) enabled = status
    }
    enabled
  }

}
