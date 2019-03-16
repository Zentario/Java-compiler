
%{
	#include<stdio.h>
%}

white  		[ \t\n]+
line		.*\n

digit		[0-9]+
id  		[_a-zA-Z][_a-zA-Z0-9]{0,30}

punctuation	[\{\}\(\)\[\];:]
keyword		(import|public|class|static|void|main|String|this|return|int|char|float|double|while|switch|case|break|default|continue|pass)
operator	[+|\-|\*|/|\.|=]
literal		\".*\"
relop		>|<|>=|<=|!=|==

%%

{white}			{}

{literal}		{ printf("<literal : %s>\n", yytext); }

{operator}		{ printf("<operator : %s>\n", yytext); }

{relop}			{ printf("<relop : %s>\n", yytext); }

{punctuation}	{ printf("<punctuation : %s>\n", yytext); }

{keyword}		{ printf("<keyword : %s>\n", yytext); }

{digit}			{ printf("<number : %s>\n", yytext); }

{id}			{ printf("<id : %s>\n", yytext); }

%%

int yywrap()
{
	return 1;
}


int main()
{

	yyin=fopen("out1.java","r");
	
	printf("\n");
	yylex();
	return 0;
	
}







