package org.enso.syntax.text.parser;
import org.enso.syntax.text.lexer.Token;import java.util.Stack;
import static org.enso.syntax.text.xx.Parser.Lexer.*;

/**
 * Enso lexical scanner 
 */
%%

%{

public abstract Token tokenAt(int i);
public abstract AST astFromChar(int i, char s);
public abstract AST astFromStr(int i, String s);

public abstract void appendExprSegment(AST ast);
public abstract void pushAST();
public abstract void popAST();

public AST ast;


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



  ////////////////////////////
  // Group Begin Management //
  ////////////////////////////

  private Stack<Token> groupBeginStack = new Stack<Token>();

  public final void pushGroupBegin(Token tok) {
    groupBeginStack.push(tok);
  }

  public final Token popGroupBegin() {
    return groupBeginStack.pop();
  }


  /////////
  // API //
  /////////

  public final Token token() {
      return token(1);
  }

  public final Token token(int offset) {
    return tokenAt(yychar - yylength() + offset);
  }

  public final AST ast() {
      return ast(1);
  }

  public final AST ast(int offset) {
    String text = yytext();
    return astFromChar(yychar,text.charAt(offset - 1));
  }

  private void rewind() {
    zzMarkedPos -= yylength();
  }



  private void groupBegin() {
    pushAST();
    pushGroupBegin(token());
    pushState(GROUPED);
  }

  private void groupEnd(boolean closed) {
    AST body    = ast;
    Token begin = popGroupBegin();
    popAST();
    AST group;
    if(closed) {
      group = AST.grouped(begin,body,token());
    } else {
      group = AST.grouped(begin,body);
    }
    appendExprSegment(group);
    popState();
  }

  private void blockBegin() {
    System.out.println("block begin!");
    pushAST();
    pushState(BLOCK);
  }




%}

%init{
%init}



/////////////
// Options //
/////////////

%abstract
%class Scanner2
//%int
%public
%type  AST
%line
%column
%char
%unicode
// %apiprivate
// %debug


/////////////////
// Definitions //
/////////////////

VAR = a
BLOCK_BEGIN = b
BLOCK_END = c
EOL = d
EOF = e
GROUP_BEGIN = f
GROUP_END = g
CONS = h


IDENT = {VAR} | {CONS}

////////////
// States //
////////////

%state BLOCK
%state GROUPED


%%

{IDENT}             {appendExprSegment(ast());}
{GROUP_BEGIN}       {groupBegin();}
{EOL}+{BLOCK_BEGIN} {blockBegin();}

<GROUPED> {
  {GROUP_END} {groupEnd(true);}
  [^]         {rewind(); groupEnd(false);}
}

<BLOCK> {
  {EOL}+ {System.out.println("push line!");}
}

{EOF} {}

[^] {System.out.println("UNRECOGNIZED"); System.out.println(yytext());}
