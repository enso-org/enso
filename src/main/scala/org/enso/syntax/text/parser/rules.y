%language "Java"
%name-prefix "Parser"
%define parser_class_name "Parser" 
%define public


%code imports {
package org.enso.syntax.text.xx;
import org.enso.syntax.text.parser.AST;
import org.enso.syntax.text.lexer.Token;
}

%code {
    public AST result;
}
//   public static void main (String args[]) throws IOException
//   {
//     CalcLexer l = new CalcLexer (System.in);
//     Calc p = new Calc (l);
//     p.parse ();
//   }

// }



// %define api.value.type {Token}

/* Bison Declarations */
%token <Token> VAR
%token <Token> EOL

%token <Token> BLOCK_BEGIN
%token <Token> BLOCK_END

%token <AST> CONS
%type  <AST> expr
%type  <AST> block
%type  <AST> blockBody
%type  <AST> tok

%start program

/////////////
// Grammar //
/////////////
%%
program:
  expr {result=$1;}
| /* empty */

expr:
  tok        {$$=$1;}
| expr tok   {$$=AST.app($1,$2);}
| expr block {$$=AST.app($1,$2);} 

block:
  BLOCK_BEGIN blockBody {$$=$2;}

blockBody: 
  expr EOL blockBody {$$=$1;}
| expr BLOCK_END     {$$=AST.emptyBlock();}

tok:
  VAR {$$=AST.fromToken($1);}
  



//   {
//     if ($1.intValue () != $3.intValue ())
//       yyerror ( "calc: error: " + $1 + " != " + $3);
//   }
// | exp '-' exp        { $$ = new Integer ($1.intValue () - $3.intValue ());  }
// | exp '*' exp        { $$ = new Integer ($1.intValue () * $3.intValue ());  }
// | exp '/' exp        { $$ = new Integer ($1.intValue () / $3.intValue ());  }
// | '-' exp  %prec NEG { $$ = new Integer (-$2.intValue ());                  }
// | exp '^' exp        { $$ = new Integer ((int)
//                                          Math.pow ($1.intValue (),
//                                                    $3.intValue ()));        }
// | '(' exp ')'        { $$ = $2;                                             }
// | '(' error ')'      { $$ = new Integer (1111);                             }
// | '!'                { $$ = new Integer (0); return YYERROR;                }
// | '-' error          { $$ = new Integer (0); return YYERROR;                }
// ;


%%
// class CalcLexer implements Calc.Lexer {

//   StreamTokenizer st;

//   public CalcLexer (InputStream is)
//   {
//     st = new StreamTokenizer (new InputStreamReader (is));
//     st.resetSyntax ();
//     st.eolIsSignificant (true);
//     st.whitespaceChars (9, 9);
//     st.whitespaceChars (32, 32);
//     st.wordChars (48, 57);
//   }


//   public void yyerror (String s)
//   {
//     System.err.println (s);
//   }


//   Integer yylval;

//   public Object getLVal() {
//     return yylval;
//   }

//   public int yylex () throws IOException {
//     int ttype = st.nextToken ();

//     if (ttype == st.TT_EOF)
//       return Calc.EOF;

//     else if (ttype == st.TT_EOL)
//       {

//         return (int) '\n';
//       }

//     else if (ttype == st.TT_WORD)
//       {
//         yylval = new Integer (st.sval);
//         return Calc.NUMX;
//       }

//     else
//       return st.ttype;
//   }



// }


// class Position {
//   public int line;
//   public int token;

//   public Position ()
//   {
//     line = 0;
//     token = 0;
//   }

//   public Position (int l, int t)
//   {
//     line = l;
//     token = t;
//   }

//   public boolean equals (Position l)
//   {
//     return l.line == line && l.token == token;
//   }

//   public String toString ()
//   {
//     return Integer.toString (line) + "." + Integer.toString(token);
//   }

//   public int lineno ()
//   {
//     return line;
//   }

//   public int token ()
//   {
//     return token;
//   }
// }