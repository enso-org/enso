package org.enso.syntax.text

import org.enso.data.List1
import org.enso.data.Shifted
import org.enso.data.Tree
import org.enso.flexer.Reader
import org.enso.syntax.text.AST.Block.OptLine
import org.enso.syntax.text.AST._
import org.enso.syntax.text.AST.conversions._
import org.enso.syntax.text.ast.DSL._
import org.scalatest._

class ParserTest extends FlatSpec with Matchers {

  def assertModule(input: String, result: AST): Assertion = {
    val parser  = Parser()
    val module  = parser.run(new Reader(input))
    val rmodule = parser.dropMacroMeta(module)
    assert(rmodule == result)
    assert(module.show() == new Reader(input).toString())
  }

  def assertExpr(input: String, result: AST): Assertion = {
    val parser  = Parser()
    val module  = parser.run(new Reader(input))
    val rmodule = parser.dropMacroMeta(module)
    val tail    = module.lines.tail
    if (!tail.forall(_.elem.isEmpty)) fail("Multi-line block")
    else {
      rmodule.lines.head.elem match {
        case None => fail("Empty expression")
        case Some(e) =>
          assert(e == result)
          assert(module.show() == new Reader(input).toString())
      }
    }
  }

  def assertIdentity(input: String): Assertion = {
    val module = Parser().run(new Reader(input))
    assert(module.show() == new Reader(input).toString())
  }

  implicit class TestString(input: String) {
    def parseTitle(str: String): String = {
      val maxChars = 20
      val escape   = (str: String) => str.replace("\n", "\\n")
      val str2     = escape(str)
      val str3 =
        if (str2.length < maxChars) str2
        else str2.take(maxChars) + "..."
      s"parse `$str3`"
    }

    private val testBase = it should parseTitle(input)

    def ?=(out: AST) = testBase in { assertExpr(input, out) }
    def ??=(out: Module) = testBase in { assertModule(input, out) }
    def testIdentity     = testBase in { assertIdentity(input)    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Identifiers /////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "_"      ?= "_"
  "Name"   ?= "Name"
  "name"   ?= "name"
  "name'"  ?= "name'"
  "name''" ?= "name''"
  "name'a" ?= Ident.InvalidSuffix("name'", "a")
  "name_"  ?= "name_"
  "name_'" ?= "name_'"
  "name'_" ?= Ident.InvalidSuffix("name'", "_")
  "name`"  ?= "name" $ Invalid.Unrecognized("`")

  //////////////////////////////////////////////////////////////////////////////
  //// Operators ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  import App.Section._
  import App.{Section => Sect}

  "++"   ?= Sides("++")
  "=="   ?= Sides("==")
  ":"    ?= Sides(":")
  ","    ?= Sides(",")
  "."    ?= Sides(".")
  ".."   ?= Sides("..")
  "..."  ?= Sides("...")
  ">="   ?= Sides(">=")
  "<="   ?= Sides("<=")
  "/="   ?= Sides("/=")
  "+="   ?= Mod("+")
  "-="   ?= Mod("-")
  "==="  ?= Ident.InvalidSuffix("==", "=")
  "...." ?= Ident.InvalidSuffix("...", ".")
  ">=="  ?= Ident.InvalidSuffix(">=", "=")
  "+=="  ?= Ident.InvalidSuffix("+", "==")

  //////////////////////////////////////////////////////////////////////////////
  //// Precedence + Associativity //////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "a b"            ?= ("a" $_ "b")
  "a +  b"         ?= ("a" $_ "+") $__ "b"
  "a + b + c"      ?= ("a" $_ "+" $_ "b") $_ "+" $_ "c"
  "a , b , c"      ?= "a" $_ "," $_ ("b" $_ "," $_ "c")
  "a + b * c"      ?= "a" $_ "+" $_ ("b" $_ "*" $_ "c")
  "a * b + c"      ?= ("a" $_ "*" $_ "b") $_ "+" $_ "c"
  "a+ b"           ?= ("a" $ "+") $$_ "b"
  "a +b"           ?= "a" $_ ("+" $ "b")
  "a+ +b"          ?= ("a" $ "+") $$_ ("+" $ "b")
  "*a+"            ?= ("*" $ "a") $ "+"
  "+a*"            ?= "+" $ ("a" $ "*")
  "+ <$> a <*> b"  ?= (Sides("+") $_ "<$>" $_ "a") $_ "<*>" $_ "b"
  "+ * ^"          ?= Sect.Right("+", 1, Sect.Right("*", 1, Sides("^")))
  "+ ^ *"          ?= Sect.Right("+", 1, Sect.Left(Sides("^"), 1, "*"))
  "^ * +"          ?= Sect.Left(Sect.Left(Sides("^"), 1, "*"), 1, "+")
  "* ^ +"          ?= Sect.Left(Sect.Right("*", 1, Sides("^")), 1, "+")
  "^ + *"          ?= App.Infix(Sides("^"), 1, "+", 1, Sides("*"))
  "* + ^"          ?= App.Infix(Sides("*"), 1, "+", 1, Sides("^"))
  "a = b.c.d = 10" ?= "a" $_ "=" $_ (("b" $ "." $ "c" $ "." $ "d") $_ "=" $_ 10)
  "v = f x=1 y=2"  ?= "v" $_ "=" $_ ("f" $_ ("x" $ "=" $ 1) $_ ("y" $ "=" $ 2))
  "v' = v .x=1"    ?= "v'" $_ "=" $_ ("v" $_ ("." $ "x" $ "=" $ 1))

  //////////////////////////////////////////////////////////////////////////////
  //// Arrows //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "a -> b"         ?= "a" $_ "->" $_ "b"
  "a -> b -> c"    ?= "a" $_ "->" $_ ("b" $_ "->" $_ "c")
  "a b -> c d"     ?= ("a" $_ "b") $_ "->" $_ ("c" $_ "d")
  "a b-> c d"      ?= "a" $_ ("b" $_ "->" $_ ("c" $_ "d"))
  "a = b -> c d"   ?= "a" $_ "=" $_ ("b" $_ "->" $_ ("c" $_ "d"))
  "a = b-> c d"    ?= "a" $_ "=" $_ ("b" $_ "->" $_ ("c" $_ "d"))
  "a + b -> c d"   ?= ("a" $_ "+" $_ "b") $_ "->" $_ ("c" $_ "d")
  "a + b-> c d"    ?= "a" $_ "+" $_ ("b" $_ "->" $_ ("c" $_ "d"))
  "a + b-> c = d"  ?= "a" $_ "+" $_ ("b" $_ "->" $_ ("c" $_ "=" $_ "d"))
  "a = b -> c = d" ?= "a" $_ "=" $_ ("b" $_ "->" $_ ("c" $_ "=" $_ "d"))

  //////////////////////////////////////////////////////////////////////////////
  //// Layout //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "" ??= Module(OptLine())
  "\n" ??= Module(OptLine(), OptLine())
  "  \n " ??= Module(OptLine(2), OptLine(1))
  "\n\n" ??= Module(OptLine(), OptLine(), OptLine())
  " \n  \n   " ??= Module(OptLine(1), OptLine(2), OptLine(3))

  //////////////////////////////////////////////////////////////////////////////
  //// Numbers /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "7"     ?= 7
  "07"    ?= Number("07")
  "10_7"  ?= Number(10, 7)
  "16_ff" ?= Number(16, "ff")
  "16_"   ?= Number.DanglingBase("16")
  "7.5"   ?= App.Infix(7, 0, Opr("."), 0, 5)

  //////////////////////////////////////////////////////////////////////////////
  //// UTF Surrogates //////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "\uD800\uDF1E" ?= Invalid.Unrecognized("\uD800\uDF1E")

  //////////////////////////////////////////////////////////////////////////////
  //// Text ////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  import Text.Segment.implicits.txtFromString

  val q1 = Text.Quote.Single
  val q3 = Text.Quote.Triple

  "'"       ?= Text.Unclosed(Text(Text.Body(q1)))
  "''"      ?= Text(Text.Body(q1))
  "'''"     ?= Text.Unclosed(Text(Text.Body(q3)))
  "''''"    ?= Text.Unclosed(Text(Text.Body(q3, "'")))
  "'''''"   ?= Text.Unclosed(Text(Text.Body(q3, "''")))
  "''''''"  ?= Text(Text.Body(q3))
  "'''''''" ?= Text(Text.Body(q3)) $ Text.Unclosed(Text(Text.Body(q1)))
  "'a'"     ?= Text(Text.Body(q1, "a"))
  "'a"      ?= Text.Unclosed(Text(Text.Body(q1, "a")))
  "'\"'"    ?= Text(Text.Body(q1, "\""))
  "'a'''"   ?= Text(Text.Body(q1, "a")) $ Text(Text.Body(q1))
  "'''a'''" ?= Text(Text.Body(q3, "a"))
  "'''a'"   ?= Text.Unclosed(Text(Text.Body(q3, "a'")))
  "'''a''"  ?= Text.Unclosed(Text(Text.Body(q3, "a''")))

  "\""            ?= Text.Unclosed(Text.Raw(Text.Body(q1)))
  "\"\""          ?= Text.Raw(Text.Body(q1))
  "\"\"\""        ?= Text.Unclosed(Text.Raw(Text.Body(q3)))
  "\"\"\"\""      ?= Text.Unclosed(Text.Raw(Text.Body(q3, "\"")))
  "\"\"\"\"\""    ?= Text.Unclosed(Text.Raw(Text.Body(q3, "\"\"")))
  "\"\"\"\"\"\""  ?= Text.Raw(Text.Body(q3))
  "\"a\""         ?= Text.Raw(Text.Body(q1, "a"))
  "\"a"           ?= Text.Unclosed(Text.Raw(Text.Body(q1, "a")))
  "\"a\"\"\""     ?= Text.Raw(Text.Body(q1, "a")) $ Text.Raw(Text.Body(q1))
  "\"\"\"a\"\"\"" ?= Text.Raw(Text.Body(q3, "a"))
  "\"\"\"a\""     ?= Text.Unclosed(Text.Raw(Text.Body(q3, "a\"")))
  "\"\"\"a\"\""   ?= Text.Unclosed(Text.Raw(Text.Body(q3, "a\"\"")))
  "\"\"\"\"\"\"\"" ?= Text.Raw(Text.Body(q3)) $ Text.Unclosed(
    Text.Raw(Text.Body(q1))
  )

  "'''\nX\n Y\n'''" ?= Text(
    Text.BodyOf(
      q3,
      List1(
        Text.LineOf(0, Nil),
        Text.LineOf(0, List("X")),
        Text.LineOf(1, List("Y")),
        Text.LineOf(0, Nil)
      )
    )
  )

  //// Escapes ////

  Text.Segment.Escape.Character.codes.foreach(
    i => s"'\\$i'" ?= Text(Text.Body(q1, Text.Segment._Escape(i)))
  )
  Text.Segment.Escape.Control.codes.foreach(
    i => s"'\\$i'" ?= Text(Text.Body(q1, Text.Segment._Escape(i)))
  )

  "'\\\\'" ?= Text(
    Text.Body(q1, Text.Segment._Escape(Text.Segment.Escape.Slash))
  )
  "'\\''" ?= Text(
    Text.Body(q1, Text.Segment._Escape(Text.Segment.Escape.Quote))
  )
  "'\\\"'" ?= Text(
    Text.Body(q1, Text.Segment._Escape(Text.Segment.Escape.RawQuote))
  )
  "'\\" ?= Text.Unclosed(Text(Text.Body(q1, "\\")))
  "'\\c'" ?= Text(
    Text.Body(q1, Text.Segment._Escape(Text.Segment.Escape.Invalid("c")))
  )
  "'\\cd'" ?= Text(
    Text.Body(q1, Text.Segment._Escape(Text.Segment.Escape.Invalid("c")), "d")
  )
  "'\\123d'" ?= Text(
    Text.Body(q1, Text.Segment._Escape(Text.Segment.Escape.Number(123)), "d")
  )

  //// Interpolation ////

  "'a`b`c'" ?= Text(Text.Body(q1, "a", Text.Segment._Expr(Some("b")), "c"))
  "'a`b 'c`d`e' f`g'" ?= {
    val bd = "b" $_ Text(Text.Body(q1, "c", Text.Segment._Expr(Some("d")), "e")) $_ "f"
    Text(Text.Body(q1, "a", Text.Segment._Expr(Some(bd)), "g"))
  }

////  //  // Comments
//////    expr("#"              , Comment)
//////    expr("#c"             , Comment :: CommentBody("c"))
////  //  expr("#c\na"          , Comment :: CommentBody("c") :: EOL :: Var("a"))
////  //  expr("#c\n a"         , Comment :: CommentBody("c") :: EOL :: CommentBody(" a"))
////  //  expr(" #c\n a"        , Comment :: CommentBody("c") :: EOL :: Var("a"))
////  //  expr(" #c\n  a"       , Comment :: CommentBody("c") :: EOL :: CommentBody("  a"))
////  //  expr("a#c"            , Var("a") :: Comment :: CommentBody("c"))
////  //  expr("a # c"          , Var("a") :: Comment :: CommentBody(" c"))
////  //  expr("a#"             , Var("a") :: Comment)
////  //  expr("a#\nb"          , Var("a") :: Comment :: EOL :: Var("b"))
////  //  expr("a#\n b"         , Var("a") :: Comment :: EOL :: CommentBody(" b"))
////  //
////  //  // Disabled
////  //  expr("a #= b"         , Var("a") :: DisabledAssignment :: Var("b"))
////  //
//
//  //////////////////////////////////////////////////////////////////////////////
//  //// Comments ////////////////////////////////////////////////////////////////
//  //////////////////////////////////////////////////////////////////////////////
//
////  "foo   ##L1"      ?= "foo" $___ Comment.SingleLine("L1")
////  "##\n    L1\n L2" ?= Comment.MultiLine(0, List("", "    L1", " L2"))
////  "##L1\nL2" ??= Module(OptLine(Comment.SingleLine("L1")), OptLine(Cons("L2")))
////  "foo #a b" ?= "foo" $_ Comment.Disable("a" $_ "b")
//
  //////////////////////////////////////////////////////////////////////////////
  //// Flags ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "x = skip a"             ?= "x" $_ "=" $_ "a"
  "x = skip a.fn"          ?= "x" $_ "=" $_ "a"
  "x = skip fn a"          ?= "x" $_ "=" $_ "a"
  "x = skip (a)"           ?= "x" $_ "=" $_ "a"
  "x = skip (a.fn)"        ?= "x" $_ "=" $_ "a"
  "x = skip (a + b)"       ?= "x" $_ "=" $_ "a"
  "x = skip ((a + b) + c)" ?= "x" $_ "=" $_ "a"
  "x = skip ()"            ?= "x" $_ "=" $_ Group()
//  "a = freeze b c" ?= "a" $_ "#=" $_ ("b" $_ "c") // freeze

  //////////////////////////////////////////////////////////////////////////////
  //// Mixfixes ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  def amb(head: AST, lst: List[List[AST]]): Macro.Ambiguous =
    Macro.Ambiguous(
      Shifted.List1(Macro.Ambiguous.Segment(head)),
      Tree(lst.map(_ -> (())): _*)
    )

  def amb(head: AST, lst: List[List[AST]], body: SAST): Macro.Ambiguous =
    Macro.Ambiguous(
      Shifted.List1(Macro.Ambiguous.Segment(head, Some(body))),
      Tree(lst.map(_ -> (())): _*)
    )

  def _amb_group_(i: Int)(t: AST): Macro.Ambiguous =
    amb("(", List(List(")")), Shifted(i, t))

  val amb_group                 = _amb_group_(0)(_)
  val amb_group_                = _amb_group_(1)(_)
  val amb_group__               = _amb_group_(2)(_)
  def group_(): Macro.Ambiguous = amb("(", List(List(")")))

  def _amb_if(i: Int)(t: AST) =
    amb("if", List(List("then"), List("then", "else")), Shifted(i, t))

  val amb_if   = _amb_if(0)(_)
  val amb_if_  = _amb_if(1)(_)
  val amb_if__ = _amb_if(2)(_)

  "()"          ?= Group()
  "( )"         ?= Group()
  "( (  )   )"  ?= Group(Group())
  "(a)"         ?= Group("a")
  "((a))"       ?= Group(Group("a"))
  "(((a)))"     ?= Group(Group(Group("a")))
  "( (  a   ))" ?= Group(Group("a"))
  "(a) (b)"     ?= Group("a") $_ Group("b")
  "("           ?= amb("(", List(List(")")))
  "(("          ?= amb_group(group_())

  "import Std .  Math  .Vector".stripMargin ?= Import("Std", "Math", "Vector")

  """def Maybe a
    |    def Just val:a
    |    def Nothing""".stripMargin ?= {
    val defJust    = Def("Just", List("val" $ ":" $ "a"))
    val defNothing = Def("Nothing")
    Def(
      "Maybe",
      List("a"),
      Some(
        Block(
          Block.Continuous,
          4,
          Block.Line(defJust),
          List(Block.Line(Some(defNothing)))
        )
      )
    )
  }
//
//  """foo ->
//    |    bar
//    |""".stripMargin ?= "foo" $_ "->" $_ Block(
//    Block.Discontinuous,
//    4,
//    "bar",
//    None
//  )
//
  "if a then b" ?= Mixfix(
    List1[AST.Ident]("if", "then"),
    List1[AST]("a", "b")
  )
  "if a then b else c" ?= Mixfix(
    List1[AST.Ident]("if", "then", "else"),
    List1[AST]("a", "b", "c")
  )

  "if a"          ?= amb_if_("a": AST)
  "(if a) b"      ?= Group(amb_if_("a": AST)) $_ "b"
  "if (a then b " ?= amb_if_(amb_group("a" $_ "then" $_ "b"))

//  //////////////////////////////////////////////////////////////////////////////
//  //// Foreign /////////////////////////////////////////////////////////////////
//  //////////////////////////////////////////////////////////////////////////////
//
//  "f = foreign Python3\n a" ?= "f" $_ "=" $_ Foreign(1, "Python3", List("a"))
//
//  val pyLine1 = "import re"
//  val pyLine2 = """re.match(r"[^@]+@[^@]+\.[^@]+", "foo@ds.pl") != None"""
//  s"""validateEmail address = foreign Python3
//     |    $pyLine1
//     |    $pyLine2""".stripMargin ?= ("validateEmail" $_ "address") $_ "=" $_
//  Foreign(4, "Python3", List(pyLine1, pyLine2))

  //////////////////////////////////////////////////////////////////////////////
  //// Large Input /////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

//  ("(" * 33000).testIdentity // FIXME Stack Overflow
//  ("OVERFLOW " * 5000).testIdentity
//  ("\uD800\uDF1E " * 10000).testIdentity

  //////////////////////////////////////////////////////////////////////////////
  //// OTHER (TO BE PARTITIONED)////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "\na \nb \n".testIdentity
  "f =  \n\n\n".testIdentity
  "  \n\n\n f\nf".testIdentity
  "f =  \n\n  x ".testIdentity
  "  a\n   b\n  c".testIdentity
  "f =\n\n  x\n\n y".testIdentity

  """
    a
     b
   c
    d
  e
   f g h
  """.testIdentity

  """
  # pop1: adults
  # pop2: children
  # pop3: mutants
    Selects the 'fittest' individuals from population and kills the rest!

  log
  '''
  keepBest
  `pop1`
  `pop2`
  `pop3`
  '''

  unique xs
    = xs.at(0.0) +: [1..length xs -1] . filter (isUnique xs) . map xs.at

  isUnique xs i ####
    = xs.at(i).score != xs.at(i-1).score

  pop1<>pop2<>pop3 . sorted . unique . take (length pop1) . pure
  """.testIdentity

  ///////////////////////
  //// Preprocessing ////
  ///////////////////////

  "\t" ??= Module(OptLine(4))
  "\r" ??= Module(OptLine(), OptLine())
  "\r\n" ??= Module(OptLine(), OptLine())

}
////////////////////////////////////////////////////////////////////////////
// TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO  //
////////////////////////////////////////////////////////////////////////////

// [ ] operator blocks
// [ ] warnings in scala code
// [ ] Undefined parsing
// [ ] All block types
// [ ] Unary minus
