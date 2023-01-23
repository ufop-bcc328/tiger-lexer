// parser.mly

%token <int> INT
%token <string> STR
%token <Symbol.symbol> ID
%token FOR
%token WHILE
%token BREAK
%token LET
%token IN
%token NIL
%token TO
%token END
%token FUNCTION
%token VAR
%token TYPE
%token ARRAY
%token IF
%token THEN
%token ELSE
%token DO
%token OF
%token LPAREN "("
%token RPAREN ")"
%token LBRACK "["
%token RBRACK "]"
%token LBRACE "{"
%token RBRACE "}"
%token DOT "."
%token COLON ":"
%token COMMA ","
%token SEMICOLON ";"
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIV "/"
%token UMINUS
%token EQ "="
%token NE "<>"
%token LT "<"
%token LE "<="
%token GT ">"
%token GE ">="
%token AND
%token OR
%token ASSIGN ":="
%token EOF

%%
