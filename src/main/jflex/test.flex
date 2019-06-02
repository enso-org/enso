package org.enso.syntax.text.lexer;
import java.util.Stack;

/**
 * Enso scanner 
 */
%%

%{
  private int comment_count = 0;
  private int lastOffset    = 0;

  private Stack<Integer> zzLexicalStateStack = new Stack<Integer>();

  public final void pushState(int state) {
    zzLexicalStateStack.push(zzLexicalState);
    yybegin(state);
  }

  public final void popState() {
    yybegin(zzLexicalStateStack.pop());
  }

  public boolean done(){
    return zzAtEOF;
  }

  public Token lex() throws java.io.IOException {
    return yylex();
  }

  public Token token(Symbol symbol) {
    int offset = lastOffset;
    lastOffset = 0;
    return new Token(symbol,offset,yylength()); 
  }

  public Token var()              { return token(new Var  (yytext())); }
  public Token cons()             { return token(new Cons (yytext())); }
  public Token wildcard()         { return token(Wildcard$.MODULE$);   }
  public Token unexpectedSuffix() { return token(invalid(new UnexpectedSuffix (yytext()))); }
  public Token operator()         { return token(new Operator (yytext())); }
  public Token modifier()         { return token(new Modifier (yytext())); }
  public Token groupBegin()       { return token(GroupBegin$.MODULE$); }
  public Token groupEnd()         { return token(GroupEnd$.MODULE$); }
  public Token listBegin()        { return token(ListBegin$.MODULE$); }
  public Token listEnd()          { return token(ListEnd$.MODULE$); }
  public Token recordBegin()      { return token(RecordBegin$.MODULE$); }
  public Token recordEnd()        { return token(RecordEnd$.MODULE$); }
  public Token unmatched()        { return token(new Unmatched(yytext())); }
  public Symbol invalid(InvalidReason reason) { 
    return new Invalid (reason); 
  }
%} 

%class Lexer
%type  Token
%line
%column
%char
%state COMMENT
%unicode
// %debug
%apiprivate

// Prims
digit           = [0-9]
alpha_upper     = [A-Z]
alpha_lower     = [a-z]
alpha           = {alpha_lower} | {alpha_upper}
alphanum        = {alpha} | digit
NONNEWLINE_WHITE_SPACE_CHAR=[\ \t\b\012]
NEWLINE=\r|\n|\r\n

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


// States
%state AFTER_IDENT
%state AFTER_OPERATOR

%% 

<YYINITIAL> {

  {NONNEWLINE_WHITE_SPACE_CHAR}+ { lastOffset = yytext().length(); }

  // Identifiers
  {ident_var}  { pushState(AFTER_IDENT); return var();}  
  {ident_cons} { pushState(AFTER_IDENT); return cons();}  
  {wildcard}   { pushState(AFTER_IDENT); return wildcard();}

  // Operators
  {operator}   { pushState(AFTER_OPERATOR); return operator(); }
  (\=)         { pushState(AFTER_OPERATOR); return operator(); }
  (\=\=)       { pushState(AFTER_OPERATOR); return operator(); }
  (\>\=)       { pushState(AFTER_OPERATOR); return operator(); }
  (\<\=)       { pushState(AFTER_OPERATOR); return operator(); }
  (\/\=)       { pushState(AFTER_OPERATOR); return operator(); }
  (\,)         { pushState(AFTER_OPERATOR); return operator(); }
  (\.)         { return operator(); }
  (\.\.)       { pushState(AFTER_OPERATOR); return operator(); }
  (\.\.\.)     { pushState(AFTER_OPERATOR); return operator(); }
  {modifier}   { pushState(AFTER_OPERATOR); return modifier(); }

  // Layout
  (\()         { return groupBegin(); }
  (\))         { return groupEnd();   }
  (\[)         { return listBegin();  }
  (\])         { return listEnd();    }
  (\{)         { return recordBegin();  }
  (\})         { return recordEnd();    }
}

<AFTER_IDENT> {
  {ident_unexpected_sfx} { return unexpectedSuffix(); }
  .                      { yypushback(1); popState(); }
}

<AFTER_OPERATOR> {
  {operator_unexpected_sfx} { return unexpectedSuffix(); }
  .                         { yypushback(1); popState(); }
}


{NEWLINE} { lastOffset = yytext().length(); }

. {
	return unmatched();
}
