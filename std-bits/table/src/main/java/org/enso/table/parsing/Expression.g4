grammar Expression;
prog:   expr EOF ;
expr:   expr POWER expr
    |   expr (MULTIPLY|DIVIDE|MODULO) expr
    |   expr (ADD|MINUS) expr
    |   expr (EQUALS|NOT_EQUALS|LESS_THAN_OR_EQUAL|GREATER_THAN_OR_EQUAL|LESS_THAN|GREATER_THAN) expr
    |   expr (IS_NULL|IS_EMPTY)
    |   expr LIKE expr
    |   expr IN '(' expr (',' expr)* ')'
    |   expr BETWEEN expr AND expr
    |   UNARY_NOT expr
    |   expr AND expr
    |   expr OR expr
    |   (NULL | NOTHING | TRUE | FALSE)
    |   IDENTIFIER '(' (expr (',' expr)*)? ')'
    |   '(' expr ')'
    |   MINUS expr
    |   COLUMN_NAME
    |   value
    ;

POWER : '^';
MULTIPLY : '*';
DIVIDE : '/';
MODULO : '%';
ADD : '+';
MINUS : '-';
EQUALS : '==' | '=';
NOT_EQUALS : '!=' | '<>';
LESS_THAN_OR_EQUAL : '<=';
GREATER_THAN_OR_EQUAL : '>=';
LESS_THAN : '<';
GREATER_THAN : '>';

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
fragment IS : I S;
fragment EMPTY : E M P T Y;

AND : A N D ;
OR : O R ;
NULL : N U L L;
NOTHING : N O T H I N G;
IS_NULL: IS ' ' (NOTHING | NULL);
IS_EMPTY: IS ' ' EMPTY;
LIKE : L I K E;
IN : I N;
BETWEEN : B E T W E E N;
TRUE : T R U E;
FALSE : F A L S E;

IDENTIFIER : LETTER (LETTER|DIGIT|'_')*;

EXCEL_STRING : '"' ('""'|~'"')* '"';

PYTHON_STRING : '\'' (ESC|~('\''|'\\'))* '\'';
fragment ESC : '\\' (["\\/bfnrt] | UNICODE);
fragment UNICODE : 'u' HEX HEX HEX HEX;
fragment HEX : [0-9a-fA-F];

fragment YEAR : DIGIT DIGIT DIGIT DIGIT;
fragment DATE_PART : '-' DIGIT DIGIT;
fragment HOUR : DIGIT DIGIT;
fragment TIME_PART : ':' DIGIT DIGIT;
fragment UTCOFFSET : ('Z' | ('+'|'-') HOUR TIME_PART?);
fragment TIMEZONE : '[' (~']')+ ']';

DATE : '#' YEAR DATE_PART DATE_PART '#' ;
TIME : '#' HOUR TIME_PART TIME_PART? '#' ;
DATE_TIME : '#' YEAR DATE_PART DATE_PART ('T' | ' ') HOUR TIME_PART TIME_PART? UTCOFFSET? TIMEZONE? '#' ;
// Doesn't include timezone and shift yet

fragment INTEGER : '0' | [1-9] (DIGIT | '_')* ;
NUMBER : '-'? INTEGER ('.' INTEGER)? ;

value
    :   NOTHING
    |   NULL
    |   TRUE
    |   FALSE
    |   DATE
    |   TIME
    |   DATE_TIME
    |   NUMBER
    |   EXCEL_STRING
    |   PYTHON_STRING
    ;

COLUMN_NAME : '[' (']]'|~']')* ']';
