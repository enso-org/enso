package org.enso.syntax.text.lexer;
import java.util.Stack;

/**
 * Enso lexical scanner 
 */
%%

%{
  /////////////////////////////
  // Lexing State Management //
  /////////////////////////////

  private Stack<Integer> zzLexicalStateStack = new Stack<Integer>();

  public final void pushState(int state) {
    zzLexicalStateStack.push(zzLexicalState);
    yybegin(state);
  }

  public final void popState() {
    yybegin(zzLexicalStateStack.pop());
  }


  ///////////////////////
  // Quote Managemenet //
  ///////////////////////

  private Stack<Integer> stringQuoteSizeStack = new Stack<Integer>();
  
  public final void pushQuoteSize(int quote) {
    stringQuoteSizeStack.push(quote);
  }
  
  public final Integer popQuoteSize() {
    return stringQuoteSizeStack.pop();
  }

  public final Integer quoteSize() {
    return stringQuoteSizeStack.peek();
  }
  

  /////////////////////////////////////////////
  // Record / Text Interpolation Managemenet //
  /////////////////////////////////////////////
  /**
   * Handles such cases as `a = '#{{}}'`
   */

  private Stack<Integer> recordNestStack = new Stack<Integer>();
  
  public final void pushRecordNest() {
    recordNestStack.push(0);
  }
  
  public final void popRecordNest() {
    recordNestStack.pop();
  }

  public final Integer recordNest() {
    return recordNestStack.peek();
  }

  public final void incrementRecordNest() {
    Integer i = recordNestStack.pop();
    recordNestStack.push(i+1);
  }

  public final void decrementRecordNest() {
    Integer i = recordNestStack.pop();
    recordNestStack.push(i-1);
  }
  

  ////////////////////
  // Number Parsing //
  ////////////////////

  private String numberPart1 = "";
  private String numberPart2 = "";
  private String numberPart3 = "";


  /////////
  // API //
  /////////

  public boolean done(){
    return zzAtEOF;
  }

  public Token lex() throws java.io.IOException {
    return yylex();
  }

  private int lastOffset = 0;
  public Token token(Symbol symbol) {
    int offset = lastOffset;
    lastOffset = 0;
    return new Token(symbol,offset,yylength()); 
  }


  //////////////////
  // Constructors //
  //////////////////

  public Token var()              { return token(new Var  (yytext())); }
  public Token cons()             { return token(new Cons (yytext())); }
  public Token wildcard()         { return token(Wildcard$.MODULE$);   }
  public Token unexpectedSuffix() { return token(invalid(new UnexpectedSuffix (yytext()))); }
  public Token operator()         { return token(new Operator (yytext())); }
  public Token modifier()         { return token(new Modifier (yytext())); }

  public Token newline()          { return token(EOL$.MODULE$); }
  public Token groupBegin()       { return token(GroupBegin$.MODULE$); }
  public Token groupEnd()         { return token(GroupEnd$.MODULE$); }
  public Token listBegin()        { return token(ListBegin$.MODULE$); }
  public Token listEnd()          { return token(ListEnd$.MODULE$); }
  public Token recordBegin()      { return token(RecordBegin$.MODULE$); }
  public Token recordEnd()        { return token(RecordEnd$.MODULE$); }
  public Token unmatched()        { return token(new Unmatched(yytext())); }

  // Numbers
  public Token number() {
    Token num = token(Number.fromString(numberPart1,numberPart2,numberPart3));
    numberPart1 = "";
    numberPart2 = "";
    numberPart3 = "";
    return num;
  }

  // Strings
  public Token textBegin()        { return token(TextBegin$.MODULE$); }
  public Token textEnd()          { return token(TextEnd$.MODULE$); }
  public Token textRawBegin()     { return token(TextRawBegin$.MODULE$); }
  public Token textRawEnd()       { return token(TextRawEnd$.MODULE$); }
  // public Token quote()            { return token(Quote$.MODULE$); }
  // public Token quoteRaw()         { return token(QuoteRaw$.MODULE$); }
  public Token text()             { return token(new Text(yytext())); }
  public Token slashEscape()      { return token(new TextEscape(SlashEscape$.MODULE$)); }
  public Token quoteEscape()      { return token(new TextEscape(QuoteEscape$.MODULE$)); }
  public Token rawQuoteEscape()   { return token(new TextEscape(RawQuoteEscape$.MODULE$)); }
  public Token intEscape()        { return token(new TextEscape(IntEscape.fromString(yytext().substring(1)))); }
  public Token charEscape(char c) { return token(new TextEscape(CharEscape.fromChar(c))); }
  public Token ctrlEscape(int c)  { return token(new TextEscape(new CtrlEscape(c))); }
  public Token uni16Escape()      { 
    String scode = yytext().substring(2);
    return token(new TextEscape(new Uni16Escape (Integer.parseInt(scode,16)))); 
  }
  public Token uni32Escape()      { return token(new TextEscape(Uni32Escape.fromString(yytext().substring(2)))); }
  public Token uni21Escape()      { 
    String scode = yytext();
    scode = scode.substring(3,scode.length()-1);
    return token(new TextEscape(Uni21Escape.fromString(scode))); }
  public Token invalidCharEscape(){ return token(new TextEscape(new InvalidCharEscape(yytext().charAt(1)))); }

  public Token textInterpolateBegin() { return token(TextInterpolateBegin$.MODULE$); }
  public Token textInterpolateEnd()   { return token(TextInterpolateEnd$.MODULE$); }

  public Symbol invalid(InvalidReason reason) { 
    return new Invalid (reason); 
  }
%} 

%init{
  recordNestStack.push(0);
%init}


/////////////
// Options //
/////////////

%class Scanner
%type  Token
%line
%column
%char
%unicode
%apiprivate
// %debug


/////////////////
// Definitions //
/////////////////

// Prims
alpha_upper     = [A-Z]
alpha_lower     = [a-z]
alpha           = {alpha_lower} | {alpha_upper}
alphanum        = {alpha} | digit
whitespace      = [\ \t\b]
newline         = \r|\n|\r\n

// Identifiers
ident_body_char = {alphanum} | _
ident_body      = {ident_body_char}*(\')*
ident_var       = {alpha_lower}{ident_body}
ident_cons      = {alpha_upper}{ident_body}
wildcard        = _
ident_unexpected_sfx_char = [^\!\@\#\$\%\^\&\*\(\)\-\=\+\[\]\{\}\|\;\:\<\>\,\.\/\ \t\r\n\\]
ident_unexpected_sfx      = {ident_unexpected_sfx_char}+

// Operators
operator_char   = [\!\$\%\&\*\+\-\/\<\>\?\^\~\|\:\\]
operator        = {operator_char}+
modifier        = {operator}=
operator_unexpected_sfx_char = {operator_char} | (\=) | (\,) | (\.)
operator_unexpected_sfx      = {operator_unexpected_sfx_char}+

// Numbers
digit   = [0-9]
hex     = [0-9a-fA-F]
decimal = {digit}+


////////////
// States //
////////////

%xstate CHECK_IDENT_SFX
%xstate CHECK_OP_SFX
%xstate NUMBER_PHASE2
%xstate NUMBER_PHASE3
%xstate TEXT
%xstate TEXT_RAW
%xstate TEXT_ESCAPE

%state  TEXT_INTERPOLATE


%% 
///////////
// Rules //
///////////

<TEXT_INTERPOLATE> {
  (\})         { 
    if(recordNest() == 0) {
      popState(); 
      popRecordNest();
      return textInterpolateEnd(); 
    } else {
      decrementRecordNest();
      return recordEnd();
    }
  }
}


//////////
// Text //
//////////

<TEXT> {
  (\')+ {
    if (yylength() == quoteSize()) {
      popState();
      popQuoteSize();
      return textEnd();
    } else {
      return text();
    }
  }

  // Prim Escapes
  (\\\\)         { return slashEscape(); }
  (\\\')         { return quoteEscape(); }
  (\\\")         { return rawQuoteEscape(); }
  (\\[0-9]+)     { return intEscape(); }

  // Escape Characters (https://en.wikipedia.org/wiki/String_literal)
  (\\a)          { return charEscape('\u0007'); } // alert
  (\\b)          { return charEscape('\u0008'); } // backspace
  (\\f)          { return charEscape('\u000C'); } // form feed
  (\\n)          { return charEscape('\n')    ; } // line feed
  (\\r)          { return charEscape('\r')    ; } // carriage return
  (\\t)          { return charEscape('\u0009'); } // horizontal tab
  (\\v)          { return charEscape('\u000B'); } // vertical tab
  (\\e)          { return charEscape('\u001B'); } // escape character
  
  // Unicode Escapes
  (\\u{hex}{hex}{hex}{hex})                     { return uni16Escape(); }
  (\\U{hex}{hex}{hex}{hex}{hex}{hex}{hex}{hex}) { return uni32Escape(); }
  (\\u\{{hex}*\})                               { return uni21Escape(); }

  // Control Characters (https://en.wikipedia.org/wiki/Control_character)
  (\\NUL)        { return ctrlEscape(0x00); }
  (\\SOH)        { return ctrlEscape(0x01); }
  (\\STX)        { return ctrlEscape(0x02); }
  (\\ETX)        { return ctrlEscape(0x03); }
  (\\EOT)        { return ctrlEscape(0x04); }
  (\\ENQ)        { return ctrlEscape(0x05); }
  (\\ACK)        { return ctrlEscape(0x06); }
  (\\BEL)        { return ctrlEscape(0x07); }
  (\\BS)         { return ctrlEscape(0x08); }
  (\\TAB)        { return ctrlEscape(0x09); }
  (\\LF)         { return ctrlEscape(0x0A); }
  (\\VT)         { return ctrlEscape(0x0B); }
  (\\FF)         { return ctrlEscape(0x0C); }
  (\\CR)         { return ctrlEscape(0x0D); }
  (\\SO)         { return ctrlEscape(0x0E); }
  (\\SI)         { return ctrlEscape(0x0F); }
  (\\DLE)        { return ctrlEscape(0x10); }
  (\\DC1)        { return ctrlEscape(0x11); }
  (\\DC2)        { return ctrlEscape(0x12); }
  (\\DC3)        { return ctrlEscape(0x13); }
  (\\DC4)        { return ctrlEscape(0x14); }
  (\\NAK)        { return ctrlEscape(0x15); }
  (\\SYN)        { return ctrlEscape(0x16); }
  (\\ETB)        { return ctrlEscape(0x17); }
  (\\CAN)        { return ctrlEscape(0x18); }
  (\\EM)         { return ctrlEscape(0x19); }
  (\\SUB)        { return ctrlEscape(0x1A); }
  (\\ESC)        { return ctrlEscape(0x1B); }
  (\\FS)         { return ctrlEscape(0x1C); }
  (\\GS)         { return ctrlEscape(0x1D); }
  (\\RS)         { return ctrlEscape(0x1E); }
  (\\US)         { return ctrlEscape(0x1F); }
  (\\DEL)        { return ctrlEscape(0x7F); }

  // Invalid Escapes
  (\\([a-z]|[A-Z])) { return invalidCharEscape(); }
  (\#)           { return text(); }
  {newline}      { return newline(); }
  [^\'\#\n\r\\]+ { return text(); }
  (\#\{)         { 
    pushState(TEXT_INTERPOLATE); 
    pushRecordNest();
    return textInterpolateBegin(); 
    }
}

<TEXT_RAW> {
  (\")+ {
    if (yylength() == quoteSize()) {
      popState();
      popQuoteSize();
      return textRawEnd();
    } else {
      return text();
    }
  }

  // Prim Escapes
  (\\\')         { return quoteEscape();    }
  (\\\")         { return rawQuoteEscape(); }
  (\\)           { return text();           }
  {newline}      { return newline();        }
  [^\"\n\r\\]+   { return text();           }

}



////////////////////////////////
// Number (e.g. 16_ff0000.ff) //
////////////////////////////////

<NUMBER_PHASE2> {
  _[a-zA-Z0-9]+ { 
    numberPart1 = numberPart2; 
    numberPart2 = yytext().substring(1); 
    popState();
    pushState(NUMBER_PHASE3); 
  }
  [^]     { yypushback(1); popState(); return number();}
  <<EOF>> { return number(); }
}

<NUMBER_PHASE3> {
  .[a-zA-Z0-9]+ { 
    numberPart3=yytext().substring(1); 
    popState(); 
    return number(); 
  }
  [^]     { yypushback(1); popState(); return number(); }
  <<EOF>> { return number(); }
}



///////////////////////
// Unexpected Suffix //
///////////////////////

<CHECK_IDENT_SFX> {
  {ident_unexpected_sfx} { return unexpectedSuffix(); }
  [^]                    { yypushback(1); popState(); }
}

<CHECK_OP_SFX> {
  {operator_unexpected_sfx} { return unexpectedSuffix(); }
  [^]                       { yypushback(1); popState(); }
}



///////////////////
// Default Rules //
///////////////////
  
// Identifiers
{ident_var}  { pushState(CHECK_IDENT_SFX); return var();}  
{ident_cons} { pushState(CHECK_IDENT_SFX); return cons();}  
{wildcard}   { pushState(CHECK_IDENT_SFX); return wildcard();}

// Operators
{operator}   { pushState(CHECK_OP_SFX); return operator(); }
(\=)         { pushState(CHECK_OP_SFX); return operator(); }
(\=\=)       { pushState(CHECK_OP_SFX); return operator(); }
(\>\=)       { pushState(CHECK_OP_SFX); return operator(); }
(\<\=)       { pushState(CHECK_OP_SFX); return operator(); }
(\/\=)       { pushState(CHECK_OP_SFX); return operator(); }
(\,)         { pushState(CHECK_OP_SFX); return operator(); }
(\.)         { return operator(); }
(\.\.)       { pushState(CHECK_OP_SFX); return operator(); }
(\.\.\.)     { pushState(CHECK_OP_SFX); return operator(); }
{modifier}   { pushState(CHECK_OP_SFX); return modifier(); }

// Layout
(\()         { return groupBegin();  }
(\))         { return groupEnd();    }
(\[)         { return listBegin();   }
(\])         { return listEnd();     }
(\{)         { incrementRecordNest(); return recordBegin(); }
(\})         { decrementRecordNest(); return recordEnd();   }

// Literals
{decimal}    { numberPart2=yytext(); pushState(NUMBER_PHASE2); }
(\')+        {
  int size = yylength(); 
  if(size == 2) {
    size = 1;
    yypushback(1);
  }
  pushQuoteSize(size); 
  pushState(TEXT);
  return textBegin(); 
}

(\")+        {
  int size = yylength(); 
  if(size == 2) {
    size = 1;
    yypushback(1);
  }
  pushQuoteSize(size); 
  pushState(TEXT_RAW);
  return textRawBegin(); 
}

{whitespace}+ { lastOffset += yytext().length(); }
{newline}     { return newline(); }

[^] {
	return unmatched();
}