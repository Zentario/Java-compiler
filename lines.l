%{
	#include<stdio.h>
	int line_num = 1;
%}

%%

.*\n 		{ printf("%d %s", line_num++, yytext); }

%%

int yywrap()
{
	return 1;
}


int main()
{

	yyin=fopen("out1.java","r");

	yylex();
	return 0;
	
}

