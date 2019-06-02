package org.enso.syntax.text.lexer

import java.io.{StringReader, StringWriter}
import collection.mutable.Stack
import org.scalatest._

class LexerSpec extends FlatSpec with Matchers {

  def lex (input:String): List[Token] = {
    var lst     = List[Token]()
    val reader  = new StringReader(input)
    val scanner = new Scanner(reader)
    do {
      val token = scanner.lex
      if (token != null) {
        lst = token +: lst
      }
    } while (!scanner.done())
    lst.reverse
  }

  def lex_ (input:String): List[Symbol] = {
    lex(input).map(tok => tok.symbol)
  }

  def assertLex (input:String, result:List[Symbol]) = {
    assert(lex_(input) == result)
  }

  def check (input:String, result:List[Symbol]) = {
    it should escape(input) in { assertLex (input, result) }
  }

  def unexpectedSuffix (input:String): Symbol = {
    Invalid(UnexpectedSuffix(input))
  }

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }

  // Identifiers
  check("_"      , Wildcard      :: Nil)
  check("Name"   , Cons("Name")  :: Nil)
  check("name"   , Var("name")   :: Nil)
  check("name'"  , Var("name'")  :: Nil)
  check("name''" , Var("name''") :: Nil)
  check("name'a" , Var("name'")  :: unexpectedSuffix("a") :: Nil)
  check("name_"  , Var("name_")  :: Nil)
  check("name_'" , Var("name_'") :: Nil)
  check("name'_" , Var("name'")  :: unexpectedSuffix("_") :: Nil)
  check("name`"  , Var("name")   :: unexpectedSuffix("`") :: Nil)
  
  // Operators
  check("="    , Operator("=")   :: Nil)
  check("=="   , Operator("==")  :: Nil)
  check("==="  , Operator("==")  :: unexpectedSuffix("=") :: Nil)
  check(":"    , Operator(":")   :: Nil)
  check(","    , Operator(",")   :: Nil)
  check("."    , Operator(".")   :: Nil)
  check(".."   , Operator("..")  :: Nil)
  check("..."  , Operator("...") :: Nil)
  check("...." , Operator("...") :: unexpectedSuffix(".") :: Nil)
  check(">="   , Operator(">=")  :: Nil)
  check("<="   , Operator("<=")  :: Nil)
  check("/="   , Operator("/=")  :: Nil)
  check("+="   , Modifier("+=")  :: Nil)
  check("-="   , Modifier("-=")  :: Nil)
  check("-=-"  , Modifier("-=")  :: unexpectedSuffix("-") :: Nil)

  // Layouts
  check("\n"   , EOL         :: Nil)
  check("\r"   , EOL         :: Nil)
  check("\r\n" , EOL         :: Nil)
  check("\n\r" , EOL         :: EOL :: Nil)
  check("(a)"  , GroupBegin  :: Var("a") :: GroupEnd  :: Nil)
  check("[a]"  , ListBegin   :: Var("a") :: ListEnd   :: Nil)
  check("{a}"  , RecordBegin :: Var("a") :: RecordEnd :: Nil)

  // Numbers
  check("7"          , Number(10,7::Nil,Nil) :: Nil)
  check("7.5"        , Number(10,7::Nil,Nil) :: Operator(".") :: Number(10,5::Nil,Nil) :: Nil)
  check("7_12"       , Number(7,1::2::Nil,Nil) :: Nil)
  check("7_12.34"    , Number(7,1::2::Nil,3::4::Nil) :: Nil)
  check("16_9acdf"   , Number(16,9::10::12::13::15::Nil,Nil) :: Nil)
  
  // Strings
  check("'"          , Quote :: Nil)
  check("''"         , Quote :: Quote :: Nil)
  check("'''"        , Quote :: Nil)
  check("' '"        , Quote :: Text(" ") :: Quote :: Nil)
  check("'' ''"      , Quote :: Quote :: Quote :: Quote :: Nil)
  check("''' '''"    , Quote :: Text(" ") :: Quote :: Nil)
  check("''' '' '''" , Quote :: Text(" ") :: Text("''") :: Text(" ") :: Quote :: Nil)
  check("'\n'"       , Quote :: EOL :: Quote :: Nil)
  check("'\\\\'"     , Quote :: TextEscape(SlashEscape) :: Quote :: Nil)
  check("'\\\''"     , Quote :: TextEscape(QuoteEscape) :: Quote :: Nil)
  check("'\\\"'"     , Quote :: TextEscape(RawQuoteEscape) :: Quote :: Nil)
  check("'\\12'"     , Quote :: TextEscape(IntEscape(12)) :: Quote :: Nil)

  check("\""          , QuoteRaw :: Nil)
  check("\"\""         , QuoteRaw :: QuoteRaw :: Nil)
  check("\"\"\""        , QuoteRaw :: Nil)
  check("\" \""        , QuoteRaw :: Text(" ") :: QuoteRaw :: Nil)
  check("\"\" \"\""      , QuoteRaw :: QuoteRaw :: QuoteRaw :: QuoteRaw :: Nil)
  check("\"\"\" \"\"\""    , QuoteRaw :: Text(" ") :: QuoteRaw :: Nil)
  check("\"\"\" \"\" \"\"\"" , QuoteRaw :: Text(" ") :: Text("\"\"") :: Text(" ") :: QuoteRaw :: Nil)
  check("\"\n\""       , QuoteRaw :: EOL :: QuoteRaw :: Nil)
  // check("\"\\\\"\""     , QuoteRaw :: TextEscape(QuoteRawEscape) :: QuoteRaw :: Nil)
  // check("\"\\\"\""     , QuoteRaw :: TextEscape(RawQuoteRawEscape) :: QuoteRaw :: Nil)
  check("\"\\12\""     , QuoteRaw :: TextEscape(IntEscape(12)) :: QuoteRaw :: Nil)

  // Char Escapes
  check("'\\a'"      , Quote :: TextEscape(CharEscape(7)) :: Quote :: Nil)
  check("'\\b'"      , Quote :: TextEscape(CharEscape(8)) :: Quote :: Nil)
  check("'\\f'"      , Quote :: TextEscape(CharEscape(12)) :: Quote :: Nil)
  check("'\\n'"      , Quote :: TextEscape(CharEscape(10)) :: Quote :: Nil)
  check("'\\r'"      , Quote :: TextEscape(CharEscape(13)) :: Quote :: Nil)
  check("'\\t'"      , Quote :: TextEscape(CharEscape(9)) :: Quote :: Nil)
  check("'\\v'"      , Quote :: TextEscape(CharEscape(11)) :: Quote :: Nil)
  check("'\\e'"      , Quote :: TextEscape(CharEscape(27)) :: Quote :: Nil)
  check("'\\q'"      , Quote :: TextEscape(InvalidCharEscape('q')) :: Quote :: Nil)

  // Control Escapes
  check("'\\NUL'"    , Quote :: TextEscape(CtrlEscape(0)) :: Quote :: Nil)
  check("'\\SOH'"    , Quote :: TextEscape(CtrlEscape(1)) :: Quote :: Nil)
  check("'\\STX'"    , Quote :: TextEscape(CtrlEscape(2)) :: Quote :: Nil)
  check("'\\ETX'"    , Quote :: TextEscape(CtrlEscape(3)) :: Quote :: Nil)
  check("'\\EOT'"    , Quote :: TextEscape(CtrlEscape(4)) :: Quote :: Nil)
  check("'\\ENQ'"    , Quote :: TextEscape(CtrlEscape(5)) :: Quote :: Nil)
  check("'\\ACK'"    , Quote :: TextEscape(CtrlEscape(6)) :: Quote :: Nil)
  check("'\\BEL'"    , Quote :: TextEscape(CtrlEscape(7)) :: Quote :: Nil)
  check("'\\BS'"     , Quote :: TextEscape(CtrlEscape(8)) :: Quote :: Nil)
  check("'\\TAB'"    , Quote :: TextEscape(CtrlEscape(9)) :: Quote :: Nil)
  check("'\\LF'"     , Quote :: TextEscape(CtrlEscape(10)) :: Quote :: Nil)
  check("'\\VT'"     , Quote :: TextEscape(CtrlEscape(11)) :: Quote :: Nil)
  check("'\\FF'"     , Quote :: TextEscape(CtrlEscape(12)) :: Quote :: Nil)
  check("'\\CR'"     , Quote :: TextEscape(CtrlEscape(13)) :: Quote :: Nil)
  check("'\\SO'"     , Quote :: TextEscape(CtrlEscape(14)) :: Quote :: Nil)
  check("'\\SI'"     , Quote :: TextEscape(CtrlEscape(15)) :: Quote :: Nil)
  check("'\\DLE'"    , Quote :: TextEscape(CtrlEscape(16)) :: Quote :: Nil)
  check("'\\DC1'"    , Quote :: TextEscape(CtrlEscape(17)) :: Quote :: Nil)
  check("'\\DC2'"    , Quote :: TextEscape(CtrlEscape(18)) :: Quote :: Nil)
  check("'\\DC3'"    , Quote :: TextEscape(CtrlEscape(19)) :: Quote :: Nil)
  check("'\\DC4'"    , Quote :: TextEscape(CtrlEscape(20)) :: Quote :: Nil)
  check("'\\NAK'"    , Quote :: TextEscape(CtrlEscape(21)) :: Quote :: Nil)
  check("'\\SYN'"    , Quote :: TextEscape(CtrlEscape(22)) :: Quote :: Nil)
  check("'\\ETB'"    , Quote :: TextEscape(CtrlEscape(23)) :: Quote :: Nil)
  check("'\\CAN'"    , Quote :: TextEscape(CtrlEscape(24)) :: Quote :: Nil)
  check("'\\EM'"     , Quote :: TextEscape(CtrlEscape(25)) :: Quote :: Nil)
  check("'\\SUB'"    , Quote :: TextEscape(CtrlEscape(26)) :: Quote :: Nil)
  check("'\\ESC'"    , Quote :: TextEscape(CtrlEscape(27)) :: Quote :: Nil)
  check("'\\FS'"     , Quote :: TextEscape(CtrlEscape(28)) :: Quote :: Nil)
  check("'\\GS'"     , Quote :: TextEscape(CtrlEscape(29)) :: Quote :: Nil)
  check("'\\RS'"     , Quote :: TextEscape(CtrlEscape(30)) :: Quote :: Nil)
  check("'\\US'"     , Quote :: TextEscape(CtrlEscape(31)) :: Quote :: Nil)
  check("'\\DEL'"    , Quote :: TextEscape(CtrlEscape(127)) :: Quote :: Nil)

  // Unicode Escapes
  check("'\\uF'"          , Quote :: TextEscape(InvalidCharEscape('u')) :: Text("F") :: Quote :: Nil)
  check("'\\uFF00'"       , Quote :: TextEscape(Uni16Escape(0xFF00)) :: Quote :: Nil)
  check("'\\U00ABCDEF'"   , Quote :: TextEscape(Uni32Escape(0x00ABCDEF)) :: Quote :: Nil)
  check("'\\UFFFFFFFF'"   , Quote :: TextEscape(InvalidUni32Escape("FFFFFFFF")) :: Quote :: Nil)
  check("'\\u{FF0}'"      , Quote :: TextEscape(Uni21Escape(0xFF0)) :: Quote :: Nil)
  check("'\\u{FFFFFFFF}'" , Quote :: TextEscape(InvalidUni21Escape("FFFFFFFF")) :: Quote :: Nil)
  check("'\\u{}'"         , Quote :: TextEscape(InvalidUni21Escape("")) :: Quote :: Nil)

  // Interpolation
  check("'#{{ }}'"        , Quote :: TextInterpolateBegin :: RecordBegin :: RecordEnd :: TextInterpolateEnd :: Quote :: Nil)
  check("'#{{ }'"         , Quote :: TextInterpolateBegin :: RecordBegin :: RecordEnd :: Quote :: Nil)
  check("'#{  }}'"        , Quote :: TextInterpolateBegin :: TextInterpolateEnd :: Text("}") :: Quote :: Nil)
  check("'#{a}'"          , Quote :: TextInterpolateBegin :: Var("a") :: TextInterpolateEnd :: Quote :: Nil)
  check("'#{'a'}'"        , Quote :: TextInterpolateBegin :: Quote :: Text("a") :: Quote :: TextInterpolateEnd :: Quote :: Nil)
  check("'''#{'a'}'''"    , Quote :: TextInterpolateBegin :: Quote :: Text("a") :: Quote :: TextInterpolateEnd :: Quote :: Nil)
  check("'#{'''a'''}'"    , Quote :: TextInterpolateBegin :: Quote :: Text("a") :: Quote :: TextInterpolateEnd :: Quote :: Nil)

}