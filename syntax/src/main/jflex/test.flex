package org.enso.syntax.text.lexer;
import java.util.Stack;
import static org.enso.syntax.text.xx.Parser.Lexer.*;

/**
 * Enso lexical scanner 
 */
%%

%{

  int currentBlock = 0;

  ///////////////////////
  // Indent Management //
  ///////////////////////

  private Stack<Integer> indentStack = new Stack<Integer>();

  public final void pushIndent(int i) {
    indentStack.push(i);
  }

  public final Integer popIndent() {
    return indentStack.pop();
  }

  public final Integer indent() {
    return indentStack.peek();
  }


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

  public int lex() throws java.io.IOException {
    return yylex();
  }

  private int lastOffset = 0;
  public Token token(Symbol symbol) {
    int offset = lastOffset;
    lastOffset = 0;
    return new Token(symbol,offset,yylength()); 
  }

  private void rewind() {
    zzMarkedPos -= yylength();
  }



  public Token value;


//////////////////
// Constructors //
//////////////////

int var()        {value = token(new Var  (yytext())); return VAR;}
int cons()       {value = token(new Cons (yytext())); return CONS;}

// Utils
void whitespace()   {lastOffset += yylength();}
//   Symbol invalid(InvalidReason reason) {
//     return new Invalid (reason); 
//   }

//   // Identifiers
//   Token var_()        {return token(new Var  (yytext()));}
//   Token cons_()       {return token(new Cons (yytext()));}
//   Token wildcard_()   {return token(Wildcard$.MODULE$);}
//   Token var()         {pushState(CHECK_IDENT_SFX); return var_();}
//   Token cons()        {pushState(CHECK_IDENT_SFX); return cons_();}
//   Token wildcard()    {pushState(CHECK_IDENT_SFX); return wildcard_();}
//   Token errorSfx()    {return token(invalid(new UnexpectedSuffix(yytext())));}
  
//   // Operators
//   Token operator_()   {return token(new Operator(yytext()));}
//   Token modifier_()   {return token(new Modifier(yytext()));}
//   Token disabled_()   {return token(DisabledAssignment$.MODULE$);}
//   Token operator()    {pushState(CHECK_OP_SFX); return operator_();}
//   Token modifier()    {pushState(CHECK_OP_SFX); return modifier_();}
//   Token disabled()    {pushState(CHECK_OP_SFX); return disabled_();}

// Layout
int blockBegin(int i)  {pushIndent(i); value = token(BlockBegin$.MODULE$); return BLOCK_BEGIN;}
int blockEnd()    {popIndent(); value = token(BlockEnd$.MODULE$); return BLOCK_END;}
int blockInvalid() {value = token(BlockInvalid$.MODULE$); return BLOCK_INVALID;}

int newline()     {value = token(EOL$.MODULE$); return EOL;}
int groupBegin()  {value = token(GroupBegin$.MODULE$); return GROUP_BEGIN;}
int groupEnd()    {value = token(GroupEnd$.MODULE$); return GROUP_END;}
//   Token groupEnd()    {return token(GroupEnd$.MODULE$);}
//   Token listBegin()   {return token(ListBegin$.MODULE$);}
//   Token listEnd()     {return token(ListEnd$.MODULE$);}
//   Token recordBegin() {return token(RecordBegin$.MODULE$);}
//   Token recordEnd()   {return token(RecordEnd$.MODULE$);}
//   Token unmatched()   {return token(new Unmatched(yytext()));}

//   // Numbers
//   Token number() {
//     Token num = token(Number.fromString(numberPart1,numberPart2,numberPart3));
//     numberPart1 = "";
//     numberPart2 = "";
//     numberPart3 = "";
//     return num;
//   }

//   // Text
//   Token textBegin()     {return token(TextBegin$.MODULE$);}
//   Token textEnd()       {return token(TextEnd$.MODULE$);}
//   Token textRawBegin()  {return token(TextRawBegin$.MODULE$);}
//   Token textRawEnd()    {return token(TextRawEnd$.MODULE$);}
//   Token text()          {return token(new Text(yytext()));}
//   Token textIntBegin()  {return token(TextInterpolateBegin$.MODULE$);}
//   Token textIntEnd()    {return token(TextInterpolateEnd$.MODULE$);}

//   // Text Escapes
//   Token slashEsc()      {return token(new TextEscape(SlashEscape$.MODULE$));}
//   Token quoteEsc()      {return token(new TextEscape(QuoteEscape$.MODULE$));}
//   Token rawQuoteEsc()   {return token(new TextEscape(RawQuoteEscape$.MODULE$));}
//   Token charEsc(char c) {return token(new TextEscape(CharEscape.fromChar(c)));}
//   Token ctrlEsc(int c)  {return token(new TextEscape(new CtrlEscape(c)));}
//   Token intEsc() { 
//     return token(new TextEscape(IntEscape.fromString(yytext().substring(1)))); 
//   }
//   Token uni16Esc() { 
//     String scode = yytext().substring(2);
//     return token(new TextEscape(new Uni16Escape (Integer.parseInt(scode,16)))); 
//   }
//   Token uni32Esc() { 
//     return token(new TextEscape(Uni32Escape.fromString(yytext().substring(2)))); 
//   }
//   Token uni21Esc() { 
//     String scode = yytext();
//     scode = scode.substring(3,scode.length()-1);
//     return token(new TextEscape(Uni21Escape.fromString(scode))); 
//   }
//   Token invalidCharEsc(){ 
//     return token(new TextEscape(new InvalidCharEscape(yytext().charAt(1)))); 
//   }

//   // Comment
//   Token comment()      {return token(Comment$.MODULE$);}
//   Token commentBody()  {return token(new CommentBody(yytext()));}
%} 

%init{
  // pushState(NEWLINE);
  pushIndent(0);
%init}



/////////////
// Options //
/////////////

%class Scanner
%int
%public
// %type  int
%line
%column
%char
%unicode
// %apiprivate
// %debug


/////////////////
// Definitions //
/////////////////

// Prims
alpha_upper = [A-Z]
alpha_lower = [a-z]
alpha       = {alpha_lower} | {alpha_upper}
alphanum    = {alpha} | {digit}
whitespace  = [\ ]
newline     = \r|\n|\r\n

// Identifiers
ident_body_char = {alphanum} | _
ident_body      = {ident_body_char}*(\')*
var             = {alpha_lower}{ident_body}
cons            = {alpha_upper}{ident_body}
wildcard        = _
ident_err_sfx_c = [^\`\!\@\#\$\%\^\&\*\(\)\-\=\+\[\]\{\}\|\;\:\<\>\,\.\/\ \t\r\n\\]
ident_err_sfx   = {ident_err_sfx_c}+

// Operators
operator_char      = [\!\$\%\&\*\+\-\/\<\>\?\^\~\|\:\\]
operator           = {operator_char}+
modifier           = {operator}=
operator_err_sfx_c = {operator_char} | (\=) | (\,) | (\.)
operator_err_sfx   = {operator_err_sfx_c}+

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
%xstate COMMENT
%xstate COMMENT_LINE
%xstate NEWLINE
%xstate BLOCK_ENDING

%state TEXT_INTERPOLATE


%% 



// ///////////////////////
// // Unexpected Suffix //
// ///////////////////////

// <CHECK_IDENT_SFX> {
//   {ident_err_sfx} {return errorSfx();}
//   [^]             {rewind(); popState();}
// }

// <CHECK_OP_SFX> {
//   {operator_err_sfx} {return errorSfx();}
//   [^]                {rewind(); popState();}
// }



// //////////
// // Text //
// //////////

// <TEXT_INTERPOLATE> {
//   (\`) {popState(); return textIntEnd();}
// }

// <TEXT> {
//   (\')+ {
//     if (yylength() == quoteSize()) {
//       popState();
//       popQuoteSize();
//       return textEnd();
//     } else {
//       return text();
//     }
//   }

//   // Prim Escapes
//   (\\\\)         {return slashEsc();}
//   (\\\')         {return quoteEsc();}
//   (\\\")         {return rawQuoteEsc();}
//   (\\[0-9]+)     {return intEsc();}

//   // Escape Characters (https://en.wikipedia.org/wiki/String_literal)
//   (\\a)          {return charEsc('\u0007');} // alert
//   (\\b)          {return charEsc('\u0008');} // backspace
//   (\\f)          {return charEsc('\u000C');} // form feed
//   (\\n)          {return charEsc('\n')    ;} // line feed
//   (\\r)          {return charEsc('\r')    ;} // carriage return
//   (\\t)          {return charEsc('\u0009');} // horizontal tab
//   (\\v)          {return charEsc('\u000B');} // vertical tab
//   (\\e)          {return charEsc('\u001B');} // escape character
  
//   // Unicode Escapes
//   (\\u{hex}{hex}{hex}{hex})                     {return uni16Esc();}
//   (\\U{hex}{hex}{hex}{hex}{hex}{hex}{hex}{hex}) {return uni32Esc();}
//   (\\u\{{hex}*\})                               {return uni21Esc();}

//   // Control Characters (https://en.wikipedia.org/wiki/Control_character)
//   (\\NUL)        {return ctrlEsc(0x00);}
//   (\\SOH)        {return ctrlEsc(0x01);}
//   (\\STX)        {return ctrlEsc(0x02);}
//   (\\ETX)        {return ctrlEsc(0x03);}
//   (\\EOT)        {return ctrlEsc(0x04);}
//   (\\ENQ)        {return ctrlEsc(0x05);}
//   (\\ACK)        {return ctrlEsc(0x06);}
//   (\\BEL)        {return ctrlEsc(0x07);}
//   (\\BS)         {return ctrlEsc(0x08);}
//   (\\TAB)        {return ctrlEsc(0x09);}
//   (\\LF)         {return ctrlEsc(0x0A);}
//   (\\VT)         {return ctrlEsc(0x0B);}
//   (\\FF)         {return ctrlEsc(0x0C);}
//   (\\CR)         {return ctrlEsc(0x0D);}
//   (\\SO)         {return ctrlEsc(0x0E);}
//   (\\SI)         {return ctrlEsc(0x0F);}
//   (\\DLE)        {return ctrlEsc(0x10);}
//   (\\DC1)        {return ctrlEsc(0x11);}
//   (\\DC2)        {return ctrlEsc(0x12);}
//   (\\DC3)        {return ctrlEsc(0x13);}
//   (\\DC4)        {return ctrlEsc(0x14);}
//   (\\NAK)        {return ctrlEsc(0x15);}
//   (\\SYN)        {return ctrlEsc(0x16);}
//   (\\ETB)        {return ctrlEsc(0x17);}
//   (\\CAN)        {return ctrlEsc(0x18);}
//   (\\EM)         {return ctrlEsc(0x19);}
//   (\\SUB)        {return ctrlEsc(0x1A);}
//   (\\ESC)        {return ctrlEsc(0x1B);}
//   (\\FS)         {return ctrlEsc(0x1C);}
//   (\\GS)         {return ctrlEsc(0x1D);}
//   (\\RS)         {return ctrlEsc(0x1E);}
//   (\\US)         {return ctrlEsc(0x1F);}
//   (\\DEL)        {return ctrlEsc(0x7F);}

//   // Invalid Escapes
//   (\\([a-z]|[A-Z])) {return invalidCharEsc();}
//   {newline}      {return newline();}
//   [^\'\`\n\r\\]+ {return text();}
//   (\`) { 
//     pushState(TEXT_INTERPOLATE); 
//     return textIntBegin(); 
//     }
// }

// <TEXT_RAW> {
//   (\")+ {
//     if (yylength() == quoteSize()) {
//       popState();
//       popQuoteSize();
//       return textRawEnd();
//     } else {
//       return text();
//     }
//   }

//   // Prim Escapes
//   (\\\')         {return quoteEsc();}
//   (\\\")         {return rawQuoteEsc();}
//   (\\)           {return text();}
//   {newline}      {return newline();}
//   [^\"\n\r\\]+   {return text();}

// }



// ////////////////////////////////
// // Number (e.g. 16_ff0000.ff) //
// ////////////////////////////////

// <NUMBER_PHASE2> {
//   _[a-zA-Z0-9]+ { 
//     numberPart1 = numberPart2; 
//     numberPart2 = yytext().substring(1); 
//     popState();
//     pushState(NUMBER_PHASE3); 
//   }
//   [^]     {rewind(); popState(); return number();}
//   <<EOF>> {return number();}
// }

// <NUMBER_PHASE3> {
//   .[a-zA-Z0-9]+ { 
//     numberPart3=yytext().substring(1); 
//     popState(); 
//     return number(); 
//   }
//   [^]     {rewind(); popState(); return number();}
//   <<EOF>> {return number();}
// }



// //////////////
// // Comments //
// //////////////

// <COMMENT> {
//   [^\n\r]+  {return commentBody();}
//   {newline} {popState(); pushState(COMMENT_LINE); return newline();}
// }

// <COMMENT_LINE> {
//   {whitespace}+ {
//     popState();
//     if(yylength() > indent) {
//       pushState(COMMENT);
//     } else {
//       pushState(NEWLINE);
//     }
//     rewind();
//   }
//   [^] {
//     popState();
//     pushState(NEWLINE);
//     rewind();
//   }
// }



///////////////////////
// Indent Management //
///////////////////////

<NEWLINE> {
  {whitespace}+{newline} {
    whitespace();
    return newline();
  }
  {whitespace}+ {
    whitespace();
    popState();
    currentBlock = yylength();
    if (currentBlock > indent()) {
      return blockBegin(currentBlock);
    } else if (currentBlock < indent()) {
      pushState(BLOCK_ENDING);
    }
  }
  [^] {
    rewind();
    popState();
    currentBlock = 0;
    if (indent() > 0) {
      pushState(BLOCK_ENDING);
    } else {
      return newline();
    }
  }
}

<BLOCK_ENDING> {

  [^] {
    rewind();
    if(currentBlock == indent()) {
      popState();
    } else if(currentBlock < indent()) {
      return blockEnd();
    } else {
      popState();
      return blockInvalid();
    }

  }

}



// ///////////////////
// // Default Rules //
// ///////////////////

  
// // Identifiers
{var}        {return var();}  
{cons}     {return cons();}  
// {wildcard} {return wildcard();}

// // Operators
// {operator} {return operator();}
// (\=)       {return operator();}
// (\=\=)     {return operator();}
// (\>\=)     {return operator();}
// (\<\=)     {return operator();}
// (\/\=)     {return operator();}
// (\,)       {return operator();}
// (\.)       {return operator_();}
// (\.\.)     {return operator();}
// (\.\.\.)   {return operator();}
// {modifier} {return modifier();}
// (\#\=)     {return disabled();}

// Layout
(\() {return groupBegin();}
(\)) {return groupEnd();}
// (\[) {return listBegin();}
// (\]) {return listEnd();}
// (\{) {return recordBegin();}
// (\}) {return recordEnd();}

// // Numbers
// {decimal} {numberPart2=yytext(); pushState(NUMBER_PHASE2);}

// // Text
// (\')+ {
//   int size = yylength(); 
//   if(size == 2) {
//     size = 1;
//     yypushback(1);
//   }
//   pushQuoteSize(size); 
//   pushState(TEXT);
//   return textBegin(); 
// }

// // Raw Text
// (\")+ {
//   int size = yylength(); 
//   if(size == 2) {
//     size = 1;
//     yypushback(1);
//   }
//   pushQuoteSize(size); 
//   pushState(TEXT_RAW);
//   return textRawBegin(); 
// }

// // Comments
// (\#) { 
//   pushState(COMMENT); 
//   return comment(); 
// }

// Layout
{whitespace}+ {whitespace();}
{newline}     {pushState(NEWLINE);return newline();}

// // Unknown
// [^] {
// 	return unmatched();
// }