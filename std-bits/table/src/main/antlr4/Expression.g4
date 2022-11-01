grammar Expression;
prog:   expr EOF ;

expr:   expr op=POWER expr                                # Power
    |   expr op=(MULTIPLY|DIVIDE|MODULO) expr             # MultDivMod
    |   expr op=(ADD|MINUS) expr                          # AddSub
    |   expr op=(EQUALS|NOT_EQUALS|LESS_THAN_OR_EQUAL|GREATER_THAN_OR_EQUAL|LESS_THAN|GREATER_THAN) expr  # Compare
    |   expr (IS_NULL|IS_EMPTY|IS_NOT_EMPTY|IS_NOT_NULL)  # IsNull
    |   expr (LIKE|NOT_LIKE) expr                         # Like
    |   expr IN '(' expr (',' expr)* ')'                  # In
    |   expr (NOT_BETWEEN | BETWEEN) expr AND expr        # Between
    |   UNARY_NOT expr                                    # UnaryNot
    |   expr op=AND expr                                  # And
    |   expr op=OR expr                                   # Or
    |   IF expr THEN expr ELSE expr END?                  # If
    |   IDENTIFIER '(' (expr (',' expr)*)? ')'            # Function
    |   '(' expr ')'                                      # Paren
    |   COLUMN_NAME                                       # Column
    |   MINUS expr                                        # UnaryMinus
    |   value                                             # Literal
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
fragment HEX : [0-9a-fA-F];
fragment IS : I S;
fragment EMPTY : E M P T Y;

AND : A N D ;
OR : O R ;
NULL : N U L L;
NOTHING : N O T H I N G;
IS_NULL: IS ' ' (NOTHING | NULL);
IS_NOT_NULL : IS ' ' N O T  ' ' (NOTHING | NULL);
IS_EMPTY: IS ' ' EMPTY;
IS_NOT_EMPTY : IS ' ' N O T  ' ' EMPTY;
LIKE : L I K E;
NOT_LIKE : N O T  ' ' LIKE;
IN : I N;
BETWEEN : B E T W E E N;
NOT_BETWEEN : N O T  ' ' BETWEEN;
TRUE : T R U E;
FALSE : F A L S E;
IF : I F;
THEN : T H E N;
ELSE : E L S E;
UNARY_NOT : (N O T) | '!';
END : E N D IF?;

IDENTIFIER : LETTER (LETTER|DIGIT|'_')*;

EXCEL_STRING : '"' ('""'|~'"')* '"';

PYTHON_STRING : '\'' (ESC|~('\''|'\\'))* '\'';
fragment ESC : '\\' (["\\/bfnrt] | UNICODE);
fragment UNICODE : 'u' HEX HEX HEX HEX;

fragment YEAR : DIGIT DIGIT DIGIT DIGIT;
fragment DATE_PART : '-' DIGIT DIGIT;
fragment HOUR : DIGIT DIGIT;
fragment TIME_PART : ':' DIGIT DIGIT;
fragment UTCOFFSET : ('Z' | ('+'|'-') HOUR TIME_PART?);
fragment TIMEZONE : '[' (~']')+ ']';

DATE : YEAR DATE_PART DATE_PART ;
TIME : HOUR TIME_PART TIME_PART? ;
DATE_TIME : YEAR DATE_PART DATE_PART ('T' | ' ') HOUR TIME_PART TIME_PART? UTCOFFSET? TIMEZONE? ;

fragment INTEGER : '0' | [1-9] (DIGIT | '_')* ;
NUMBER : INTEGER ('.' INTEGER)? ;

value
    :   (NULL | NOTHING)       # nullOrNothing
    |   (TRUE | FALSE)         # boolean
    |   '#' text=DATE '#'      # date
    |   '#' text=TIME '#'      # time
    |   '#' text=DATE_TIME '#' # datetime
    |   NUMBER                 # number
    |   EXCEL_STRING           # excelString
    |   PYTHON_STRING          # pythonString
    ;

COLUMN_NAME : '[' (']]'|~']')* ']';
