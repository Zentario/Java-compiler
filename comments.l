%{
	#include<stdio.h>
	int line_num = 1;
%}

%%

\/\/(.*) 			{}					

\/\*(.*\n)*.*\*\/  	{}


%%

int yywrap()
{
	return 1;
}


int main()
{

	yyin=fopen("test.java","r");
	yyout=fopen("out1.java","w");

	yylex();
	return 0;
	
}