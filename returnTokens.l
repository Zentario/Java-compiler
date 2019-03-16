
%{
	#include<stdio.h>
	#include "y.tab.h"
%}

white  		[ \t\n]+

digit		[0-9]+
id  		[_a-zA-Z][_a-zA-Z0-9]{0,30}

operator	[+|\-|\*|/|\.|=]
literal		\".*\"
relop		>|<|>=|<=|!=|==

%%

{white}			{}

{literal}		{ return LITERAL; }

{operator}		{ return OPERATOR; }

{relop}			{ return RELOP; }


class			{ return CLASS; }
import			{ return IMPORT; }
public			{ return PUBLIC; }
static			{ return STATIC; }
void			{ return VOID; }
this			{ return THIS; }
new 			{ return NEW; }
return 			{ return RETURN; }

int				{ return INT; }
char			{ return CHAR; }
float			{ return FLOAT; }

while			{ return WHILE; }
continue		{ return CONTINUE; }
switch			{ return SWITCH; }
case			{ return CASE; }
break			{ return BREAK; }
default			{ return DEFAULT; }

{id}			{ return IDENTIFIER; }

%%








