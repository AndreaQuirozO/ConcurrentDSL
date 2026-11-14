%{
open Ast
%}

%token <int> INT
%token <string> VAR
%token PLUS MINUS TIMES EOF LPAREN RPAREN LBRACE RBRACE
%token TRUE FALSE EQUALS NOTEQUALS LESS LESSEQ GREATER GREATEREQ
%token NOT AND OR
%token SKIP ASSIGN SEMI IF THEN ELSE WHILE DO
%token SPAWN JOIN LOCK UNLOCK CREATELOCK

%type <Ast.com> program
%start program

%%

program: com EOF                        { $1 }

/* Sequencing is right-nested; a trailing semicolon is allowed. */
com: stmt                               { $1 }
  | stmt SEMI                           { $1 }
  | stmt SEMI com                       { Seq($1, $3) }

/* if/while/spawn bodies are always braced, which avoids the dangling-else problem. */
stmt: SKIP                              { Skip }
  | VAR ASSIGN aexp                     { Assign($1, $3) }
  | VAR ASSIGN SPAWN block              { Spawn($1, $4) }
  | VAR ASSIGN CREATELOCK LPAREN RPAREN { CreateLock $1 }
  | JOIN VAR                            { Join $2 }
  | LOCK VAR                            { Lock $2 }
  | UNLOCK VAR                          { Unlock $2 }
  | IF bexp THEN block ELSE block       { If($2, $4, $6) }
  | WHILE bexp DO block                 { While($2, $4) }

block: LBRACE com RBRACE                { $2 }
  | LBRACE RBRACE                       { Skip }

aexp: aexp PLUS taexp                   { Plus($1, $3) }
  | aexp MINUS taexp                    { Minus($1, $3) }
  | taexp                               { $1 }
taexp: taexp TIMES aaexp                { Times($1, $3) }
  | aaexp                               { $1 }
aaexp: INT                              { Int $1 }
  | MINUS INT                           { Int (- $2) }
  | VAR                                 { Var $1 }
  | LPAREN aexp RPAREN                  { $2 }

bexp: bexp OR cbexp                     { Or($1, $3) }
  | cbexp                               { $1 }
cbexp: cbexp AND nbexp                  { And($1, $3) }
  | nbexp                               { $1 }
nbexp: NOT nbexp                        { Not $2 }
  | abexp                               { $1 }
abexp: TRUE                             { True }
  | FALSE                               { False }
  | aexp EQUALS aexp                    { Equals($1, $3) }
  | aexp NOTEQUALS aexp                 { NotEquals($1, $3) }
  | aexp LESS aexp                      { Less($1, $3) }
  | aexp LESSEQ aexp                    { LessEq($1, $3) }
  | aexp GREATER aexp                   { Greater($1, $3) }
  | aexp GREATEREQ aexp                 { GreaterEq($1, $3) }
  | LPAREN bexp RPAREN                  { $2 }