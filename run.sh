lex comments.l
gcc lex.yy.c -ll
./a.out

lex lines.l
gcc lex.yy.c -ll
./a.out

lex tokens.l
gcc lex.yy.c -ll
./a.out

yacc -d gram.y
lex returnTokens.l
gcc y.tab.c lex.yy.c -ll