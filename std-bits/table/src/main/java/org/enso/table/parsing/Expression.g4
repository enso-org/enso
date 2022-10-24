grammar Expression;
prog:   expr EOF ;
expr:   expr (OR) expr
    |   expr (AND) expr
    |   UNARY_NOT expr
    |   expr BETWEEN expr AND expr
    |   expr IN '(' expr (',' expr)* ')'
    |   expr LIKE expr
    |   expr (IS_NULL|IS_EMPTY)
    |   expr (EQUALS|NOT_EQUALS|'<='|'>='|'>'|'<') expr
    |   expr (ADD|SUBTRACT) expr
    |   expr (MULTIPLY|DIVIDE|MODULO) expr
    |   expr POWER expr
    |   IDENTIFIER '(' (expr (',' expr)*)? ')'
    |   '(' expr ')'
    |   '-' expr
    |   COLUMN_NAME
    |   value
    ;

POWER : '^';
MULTIPLY : '*';
DIVIDE : '/';
MODULO : '%';
ADD : '+';
SUBTRACT : '-';
EQUALS : '==' | '=';
NOT_EQUALS : '!=' | '<>';

UNARY_NOT : (N O T) | '!';

WHITESPACE : [ \t\r\n]+ -> skip;

fragment A:[aA];
fragment B:[bB];
fragment C:[cC];
fragment D:[dD];
fragment E:[eE];
fragment F:[fF];
fragment G:[gG];
fragment H:[hH];
fragment I:[iI];
fragment J:[jJ];
fragment K:[kK];
fragment L:[lL];
fragment M:[mM];
fragment N:[nN];
fragment O:[oO];
fragment P:[pP];
fragment Q:[qQ];
fragment R:[rR];
fragment S:[sS];
fragment T:[tT];
fragment U:[uU];
fragment V:[vV];
fragment W:[wW];
fragment X:[xX];
fragment Y:[yY];
fragment Z:[zZ];
fragment LETTER : [A-Za-z];
fragment DIGIT : [0-9];
IDENTIFIER : LETTER (LETTER|DIGIT|'_')*;

fragment IS : I S;
NULL : N U L L;
NOTHING : N O T H I N G;
fragment EMPTY : E M P T Y;

AND : A N D ;
OR : O R ;

IS_NULL: IS ' ' (NOTHING | NULL);
IS_EMPTY: IS ' ' EMPTY;
LIKE : L I K E;
IN : I N;

BETWEEN : B E T W E E N;

COLUMN_NAME : '[' (']]'|~']')* ']';

TRUE : T R U E;
FALSE : F A L S E;

EXCEL_STRING : '"' ('""'|~'"')* '"';

PYTHON_STRING : '\'' (ESC|~('\''|'\\'))* '\'';
fragment ESC : '\\' (["\\/bfnrt] | UNICODE);
fragment UNICODE : 'u' HEX HEX HEX HEX;
fragment HEX : [0-9a-fA-F];

DATE : '#' DIGIT DIGIT DIGIT DIGIT '-' DIGIT DIGIT '-' DIGIT DIGIT '#' ;
TIME : '#' DIGIT DIGIT ':' DIGIT DIGIT (':' DIGIT DIGIT)? '#' ;
// DATE_TIME

fragment INTEGER : '0' | [1-9] (DIGIT | '_')* ;
NUMBER : '-'? INTEGER ('.' INTEGER)? ;

value
    :   NOTHING
    |   NULL
    |   TRUE
    |   FALSE
    |   DATE
    |   TIME
    |   NUMBER
    |   EXCEL_STRING
    |   PYTHON_STRING
    ;
