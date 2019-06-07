package org.enso.syntax.text.lexer
//
//import org.scalatest._
//
//class LexerSpec extends FlatSpec with Matchers {
//
//  def lex (input:String): List[Token] = {
//    new Lexer(input).lexAll().to[List]
//  }
//
//  def lex_ (input:String): List[Symbol] = {
//    lex(input).map(tok => tok.symbol)
//  }
//
//  def assertLex (input:String, result:List[Symbol]) = {
//    assert(lex_(input) == (result :+ EOF))
//  }
//
//  def check (input:String, result:List[Symbol]) = {
//    it should escape(input) in { assertLex (input, result) }
//  }
//
//  def unexpectedSuffix (input:String): Symbol = {
//    Invalid(UnexpectedSuffix(input))
//  }
//
//  def escape(raw: String): String = {
//    import scala.reflect.runtime.universe._
//    Literal(Constant(raw)).toString
//  }
//
//
//
//  /////////////////
//  // Identifiers //
//  /////////////////
//
//  check("_"      , Wildcard      :: Nil)
//  check("Name"   , Cons("Name")  :: Nil)
//  check("name"   , Var("name")   :: Nil)
//  check("name'"  , Var("name'")  :: Nil)
//  check("name''" , Var("name''") :: Nil)
//  check("name'a" , Var("name'")  :: unexpectedSuffix("a") :: Nil)
//  check("name_"  , Var("name_")  :: Nil)
//  check("name_'" , Var("name_'") :: Nil)
//  check("name'_" , Var("name'")  :: unexpectedSuffix("_") :: Nil)
//  check("name`"  , Var("name")   :: Unmatched("`") :: Nil)
//
//
//
//  ///////////////
//  // Operators //
//  ///////////////
//
//  check("="    , Operator("=")   :: Nil)
//  check("=="   , Operator("==")  :: Nil)
//  check("==="  , Operator("==")  :: unexpectedSuffix("=") :: Nil)
//  check(":"    , Operator(":")   :: Nil)
//  check(","    , Operator(",")   :: Nil)
//  check("."    , Operator(".")   :: Nil)
//  check(".."   , Operator("..")  :: Nil)
//  check("..."  , Operator("...") :: Nil)
//  check("...." , Operator("...") :: unexpectedSuffix(".") :: Nil)
//  check(">="   , Operator(">=")  :: Nil)
//  check("<="   , Operator("<=")  :: Nil)
//  check("/="   , Operator("/=")  :: Nil)
//  check("+="   , Modifier("+=")  :: Nil)
//  check("-="   , Modifier("-=")  :: Nil)
//  check("-=-"  , Modifier("-=")  :: unexpectedSuffix("-") :: Nil)
//
//
//
//  ////////////
//  // Layout //
//  ////////////
//
//  check(""     , Nil)
//  check("\n"   , EOL         :: Nil)
//  check("\n\n" , EOL :: EOL  :: Nil)
//  check("\r"   , EOL         :: Nil)
//  check("\r\n" , EOL         :: Nil)
//  check("\n\r" , EOL         :: EOL :: Nil)
//  check("(a)"  , GroupBegin  :: Var("a") :: GroupEnd  :: Nil)
//  check("[a]"  , ListBegin   :: Var("a") :: ListEnd   :: Nil)
//  check("{a}"  , RecordBegin :: Var("a") :: RecordEnd :: Nil)
//
//
//
//  /////////////
//  // Numbers //
//  /////////////
//
//  check("7"          , Number(10,7::Nil,Nil) :: Nil)
//  check("7.5"        , Number(10,7::Nil,Nil) :: Operator(".") :: Number(10,5::Nil,Nil) :: Nil)
//  check("7_12"       , Number(7,1::2::Nil,Nil) :: Nil)
//  check("7_12.34"    , Number(7,1::2::Nil,3::4::Nil) :: Nil)
//  check("16_9acdf"   , Number(16,9::10::12::13::15::Nil,Nil) :: Nil)
//
//
//
//  //////////
//  // Text //
//  //////////
//
//  // Basic
//  check("'"          , TextBegin    :: Nil)
//  check("\""         , TextRawBegin :: Nil)
//  check("''"         , TextBegin    :: TextEnd :: Nil)
//  check("\"\""       , TextRawBegin :: TextRawEnd :: Nil)
//  check("'''"        , TextBegin    :: Nil)
//  check("\"\"\""     , TextRawBegin :: Nil)
//  check("' '"        , TextBegin    :: Text(" ") :: TextEnd :: Nil)
//  check("\" \""      , TextRawBegin :: Text(" ") :: TextRawEnd :: Nil)
//  check("'' ''"      , TextBegin    :: TextEnd    :: TextBegin    :: TextEnd    :: Nil)
//  check("\"\" \"\""  , TextRawBegin :: TextRawEnd :: TextRawBegin :: TextRawEnd :: Nil)
//  check("'\n'"       , TextBegin    :: EOL :: TextEnd    :: Nil)
//  check("\"\n\""     , TextRawBegin :: EOL :: TextRawEnd :: Nil)
//  check("'\\\\'"     , TextBegin    :: TextEscape(SlashEscape) :: TextEnd :: Nil)
//  check("\"\\\\\""   , TextRawBegin :: Text("\\") :: TextEscape(RawQuoteEscape) :: Nil)
//  check("'\\\''"     , TextBegin    :: TextEscape(QuoteEscape) :: TextEnd    :: Nil)
//  check("\"\\\'\""   , TextRawBegin :: TextEscape(QuoteEscape) :: TextRawEnd :: Nil)
//  check("'\\\"'"     , TextBegin    :: TextEscape(RawQuoteEscape) :: TextEnd    :: Nil)
//  check("\"\\\"\""   , TextRawBegin :: TextEscape(RawQuoteEscape) :: TextRawEnd :: Nil)
//  check("''' '''"            , TextBegin    :: Text(" ") :: TextEnd    :: Nil)
//  check("\"\"\" \"\"\""      , TextRawBegin :: Text(" ") :: TextRawEnd :: Nil)
//  check("''' '' '''"         , TextBegin    :: Text(" ") :: Text("''")   :: Text(" ") :: TextEnd    :: Nil)
//  check("\"\"\" \"\" \"\"\"" , TextRawBegin :: Text(" ") :: Text("\"\"") :: Text(" ") :: TextRawEnd :: Nil)
//
//  // Int Escapes
//  check("'\\12'"     , TextBegin    :: TextEscape(IntEscape(12)) :: TextEnd    :: Nil)
//  check("\"\\12\""   , TextRawBegin :: Text("\\") :: Text("12")  :: TextRawEnd :: Nil)
//
//  // Char Escapes
//  check("'\\a'"      , TextBegin :: TextEscape(CharEscape(7)) :: TextEnd :: Nil)
//  check("'\\b'"      , TextBegin :: TextEscape(CharEscape(8)) :: TextEnd :: Nil)
//  check("'\\f'"      , TextBegin :: TextEscape(CharEscape(12)) :: TextEnd :: Nil)
//  check("'\\n'"      , TextBegin :: TextEscape(CharEscape(10)) :: TextEnd :: Nil)
//  check("'\\r'"      , TextBegin :: TextEscape(CharEscape(13)) :: TextEnd :: Nil)
//  check("'\\t'"      , TextBegin :: TextEscape(CharEscape(9)) :: TextEnd :: Nil)
//  check("'\\v'"      , TextBegin :: TextEscape(CharEscape(11)) :: TextEnd :: Nil)
//  check("'\\e'"      , TextBegin :: TextEscape(CharEscape(27)) :: TextEnd :: Nil)
//  check("'\\q'"      , TextBegin :: TextEscape(InvalidCharEscape('q')) :: TextEnd :: Nil)
//  check("\"\\a\""    , TextRawBegin :: Text("\\") :: Text("a") :: TextRawEnd :: Nil)
//
//  // Control Escapes
//  check("'\\NUL'"    , TextBegin :: TextEscape(CtrlEscape(0)) :: TextEnd :: Nil)
//  check("'\\SOH'"    , TextBegin :: TextEscape(CtrlEscape(1)) :: TextEnd :: Nil)
//  check("'\\STX'"    , TextBegin :: TextEscape(CtrlEscape(2)) :: TextEnd :: Nil)
//  check("'\\ETX'"    , TextBegin :: TextEscape(CtrlEscape(3)) :: TextEnd :: Nil)
//  check("'\\EOT'"    , TextBegin :: TextEscape(CtrlEscape(4)) :: TextEnd :: Nil)
//  check("'\\ENQ'"    , TextBegin :: TextEscape(CtrlEscape(5)) :: TextEnd :: Nil)
//  check("'\\ACK'"    , TextBegin :: TextEscape(CtrlEscape(6)) :: TextEnd :: Nil)
//  check("'\\BEL'"    , TextBegin :: TextEscape(CtrlEscape(7)) :: TextEnd :: Nil)
//  check("'\\BS'"     , TextBegin :: TextEscape(CtrlEscape(8)) :: TextEnd :: Nil)
//  check("'\\TAB'"    , TextBegin :: TextEscape(CtrlEscape(9)) :: TextEnd :: Nil)
//  check("'\\LF'"     , TextBegin :: TextEscape(CtrlEscape(10)) :: TextEnd :: Nil)
//  check("'\\VT'"     , TextBegin :: TextEscape(CtrlEscape(11)) :: TextEnd :: Nil)
//  check("'\\FF'"     , TextBegin :: TextEscape(CtrlEscape(12)) :: TextEnd :: Nil)
//  check("'\\CR'"     , TextBegin :: TextEscape(CtrlEscape(13)) :: TextEnd :: Nil)
//  check("'\\SO'"     , TextBegin :: TextEscape(CtrlEscape(14)) :: TextEnd :: Nil)
//  check("'\\SI'"     , TextBegin :: TextEscape(CtrlEscape(15)) :: TextEnd :: Nil)
//  check("'\\DLE'"    , TextBegin :: TextEscape(CtrlEscape(16)) :: TextEnd :: Nil)
//  check("'\\DC1'"    , TextBegin :: TextEscape(CtrlEscape(17)) :: TextEnd :: Nil)
//  check("'\\DC2'"    , TextBegin :: TextEscape(CtrlEscape(18)) :: TextEnd :: Nil)
//  check("'\\DC3'"    , TextBegin :: TextEscape(CtrlEscape(19)) :: TextEnd :: Nil)
//  check("'\\DC4'"    , TextBegin :: TextEscape(CtrlEscape(20)) :: TextEnd :: Nil)
//  check("'\\NAK'"    , TextBegin :: TextEscape(CtrlEscape(21)) :: TextEnd :: Nil)
//  check("'\\SYN'"    , TextBegin :: TextEscape(CtrlEscape(22)) :: TextEnd :: Nil)
//  check("'\\ETB'"    , TextBegin :: TextEscape(CtrlEscape(23)) :: TextEnd :: Nil)
//  check("'\\CAN'"    , TextBegin :: TextEscape(CtrlEscape(24)) :: TextEnd :: Nil)
//  check("'\\EM'"     , TextBegin :: TextEscape(CtrlEscape(25)) :: TextEnd :: Nil)
//  check("'\\SUB'"    , TextBegin :: TextEscape(CtrlEscape(26)) :: TextEnd :: Nil)
//  check("'\\ESC'"    , TextBegin :: TextEscape(CtrlEscape(27)) :: TextEnd :: Nil)
//  check("'\\FS'"     , TextBegin :: TextEscape(CtrlEscape(28)) :: TextEnd :: Nil)
//  check("'\\GS'"     , TextBegin :: TextEscape(CtrlEscape(29)) :: TextEnd :: Nil)
//  check("'\\RS'"     , TextBegin :: TextEscape(CtrlEscape(30)) :: TextEnd :: Nil)
//  check("'\\US'"     , TextBegin :: TextEscape(CtrlEscape(31)) :: TextEnd :: Nil)
//  check("'\\DEL'"    , TextBegin :: TextEscape(CtrlEscape(127)) :: TextEnd :: Nil)
//  check("\"\\NUL\""  , TextRawBegin :: Text("\\") :: Text("NUL") :: TextRawEnd :: Nil)
//
//  // Unicode Escapes
//  check("'\\uF'"          , TextBegin :: TextEscape(InvalidCharEscape('u')) :: Text("F") :: TextEnd :: Nil)
//  check("'\\uFF00'"       , TextBegin :: TextEscape(Uni16Escape(0xFF00)) :: TextEnd :: Nil)
//  check("'\\U00ABCDEF'"   , TextBegin :: TextEscape(Uni32Escape(0x00ABCDEF)) :: TextEnd :: Nil)
//  check("'\\UFFFFFFFF'"   , TextBegin :: TextEscape(InvalidUni32Escape("FFFFFFFF")) :: TextEnd :: Nil)
//  check("'\\u{FF0}'"      , TextBegin :: TextEscape(Uni21Escape(0xFF0)) :: TextEnd :: Nil)
//  check("'\\u{FFFFFFFF}'" , TextBegin :: TextEscape(InvalidUni21Escape("FFFFFFFF")) :: TextEnd :: Nil)
//  check("'\\u{}'"         , TextBegin :: TextEscape(InvalidUni21Escape("")) :: TextEnd :: Nil)
//
//  // Interpolation
//  check("'`{ }`'"        , TextBegin :: TextInterpolateBegin :: RecordBegin :: RecordEnd :: TextInterpolateEnd :: TextEnd :: Nil)
//  check("'`{ }'"         , TextBegin :: TextInterpolateBegin :: RecordBegin :: RecordEnd :: TextBegin :: Nil)
//  check("'`  `}'"        , TextBegin :: TextInterpolateBegin :: TextInterpolateEnd :: Text("}") :: TextEnd :: Nil)
//  check("'`a`'"          , TextBegin :: TextInterpolateBegin :: Var("a") :: TextInterpolateEnd :: TextEnd :: Nil)
//  check("'`'a'`'"        , TextBegin :: TextInterpolateBegin :: TextBegin :: Text("a") :: TextEnd :: TextInterpolateEnd :: TextEnd :: Nil)
//  check("'''`'a'`'''"    , TextBegin :: TextInterpolateBegin :: TextBegin :: Text("a") :: TextEnd :: TextInterpolateEnd :: TextEnd :: Nil)
//  check("'`'''a'''`'"    , TextBegin :: TextInterpolateBegin :: TextBegin :: Text("a") :: TextEnd :: TextInterpolateEnd :: TextEnd :: Nil)
//  check("\"``\""         , TextRawBegin :: Text("``") :: TextRawEnd :: Nil)
//  check("'`'`a`'`'"      , TextBegin :: TextInterpolateBegin :: TextBegin :: TextInterpolateBegin :: Var("a") :: TextInterpolateEnd :: TextEnd :: TextInterpolateEnd :: TextEnd :: Nil)
//
//  // Comments
//  check("#"              , Comment :: Nil)
//  check("#c"             , Comment :: CommentBody("c") :: Nil)
//  check("#c\na"          , Comment :: CommentBody("c") :: EOL :: Var("a") :: Nil)
//  check("#c\n a"         , Comment :: CommentBody("c") :: EOL :: CommentBody(" a") :: Nil)
//  check(" #c\n a"        , Comment :: CommentBody("c") :: EOL :: Var("a") :: Nil)
//  check(" #c\n  a"       , Comment :: CommentBody("c") :: EOL :: CommentBody("  a") :: Nil)
//  check("a#c"            , Var("a") :: Comment :: CommentBody("c") :: Nil)
//  check("a # c"          , Var("a") :: Comment :: CommentBody(" c") :: Nil)
//  check("a#"             , Var("a") :: Comment :: Nil)
//  check("a#\nb"          , Var("a") :: Comment :: EOL :: Var("b") :: Nil)
//  check("a#\n b"         , Var("a") :: Comment :: EOL :: CommentBody(" b") :: Nil)
//
//  // Disabled
//  check("a #= b"         , Var("a") :: DisabledAssignment :: Var("b") :: Nil)
//
//}