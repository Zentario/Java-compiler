%{
	#include<stdio.h>
	int yylex();
	void yyerror(const char* msg);
%}

%token BREAK
%token CASE CHAR CLASS CONTINUE
%token DEFAULT
%token FLOAT
%token IMPORT INT
%token NEW
%token OPERATOR
%token PACKAGE PUBLIC
%token RETURN
%token STATIC SWITCH
%token THIS
%token VOID
%token WHILE
%token RELOP
%token IDENTIFIER LITERAL

%start CompilationUnit

%%

TypeSpecifier
	: TypeName
	| TypeName Dims
	;

TypeName
	: PrimitiveType
	| QualifiedName
	;

ClassNameList
        : QualifiedName
        | ClassNameList ',' QualifiedName
	;

PrimitiveType
	: CHAR
	| INT
	| FLOAT
	| VOID
	;

SemiColons
	: ';'
        | SemiColons ';'
        ;

CompilationUnit
	: ProgramFile
        ;

ProgramFile
	: PackageStatement ImportStatements TypeDeclarations
	| PackageStatement ImportStatements
	| PackageStatement                  TypeDeclarations
	|                  ImportStatements TypeDeclarations
	| PackageStatement
	|                  ImportStatements
	|                                   TypeDeclarations
	;

PackageStatement
	: PACKAGE QualifiedName SemiColons
	;

TypeDeclarations
	: TypeDeclarationOptSemi
	| TypeDeclarations TypeDeclarationOptSemi
	;

TypeDeclarationOptSemi
        : TypeDeclaration
        | TypeDeclaration SemiColons
        ;

ImportStatements
	: ImportStatement
	| ImportStatements ImportStatement
	;

ImportStatement
	: IMPORT QualifiedName SemiColons
	| IMPORT QualifiedName '.' '*' SemiColons
	;

QualifiedName
	: IDENTIFIER
	| QualifiedName '.' IDENTIFIER
	;

TypeDeclaration
	: ClassHeader '{' FieldDeclarations '}'
	| ClassHeader '{' '}'
	;

ClassHeader
	: Modifiers ClassWord IDENTIFIER 
	| Modifiers ClassWord IDENTIFIER 
	| Modifiers ClassWord IDENTIFIER 
	|           ClassWord IDENTIFIER 
	| Modifiers ClassWord IDENTIFIER
	|           ClassWord IDENTIFIER 
	|           ClassWord IDENTIFIER      
	|           ClassWord IDENTIFIER
	;

Modifiers
	: Modifier
	| Modifiers Modifier
	;

Modifier
	: PUBLIC
	| STATIC
	;

ClassWord
	: CLASS
	;


FieldDeclarations
	: FieldDeclarationOptSemi
        | FieldDeclarations FieldDeclarationOptSemi
	;

FieldDeclarationOptSemi
        : FieldDeclaration
        | FieldDeclaration SemiColons
        ;

FieldDeclaration
	: FieldVariableDeclaration ';'
	| MethodDeclaration
	| ConstructorDeclaration
	| StaticInitializer
        | NonStaticInitializer
        | TypeDeclaration
	;

FieldVariableDeclaration
	: Modifiers TypeSpecifier VariableDeclarators
	|           TypeSpecifier VariableDeclarators
	;

VariableDeclarators
	: VariableDeclarator
	| VariableDeclarators ',' VariableDeclarator
	;

VariableDeclarator
	: DeclaratorName
	| DeclaratorName '=' VariableInitializer
	;

VariableInitializer
	: Expression
	| '{' '}'
        | '{' ArrayInitializers '}'
        ;

ArrayInitializers
	: VariableInitializer
	| ArrayInitializers ',' VariableInitializer
	| ArrayInitializers ','
	;

MethodDeclaration
	: Modifiers TypeSpecifier MethodDeclarator        MethodBody
	|           TypeSpecifier MethodDeclarator        MethodBody
	;

MethodDeclarator
	: DeclaratorName '(' ParameterList ')'
	| DeclaratorName '(' ')'
	| MethodDeclarator OP_DIM
	;

ParameterList
	: Parameter
	| ParameterList ',' Parameter
	;

Parameter
	: TypeSpecifier DeclaratorName
	;

DeclaratorName
	: IDENTIFIER
        | DeclaratorName OP_DIM
        ;


MethodBody
	: Block
	| ';'
	;

ConstructorDeclaration
	: Modifiers ConstructorDeclarator        Block
	|           ConstructorDeclarator        Block
	;

ConstructorDeclarator
	: IDENTIFIER '(' ParameterList ')'
	| IDENTIFIER '(' ')'
	;

StaticInitializer
	: STATIC Block
	;

NonStaticInitializer
        : Block
        ;

Extends
	: Extends ',' TypeName
	;

Block
	: '{' LocalVariableDeclarationsAndStatements '}'
	| '{' '}'
        ;

LocalVariableDeclarationsAndStatements
	: LocalVariableDeclarationOrStatement
	| LocalVariableDeclarationsAndStatements LocalVariableDeclarationOrStatement
	;

LocalVariableDeclarationOrStatement
	: LocalVariableDeclarationStatement
	| Statement
	;

LocalVariableDeclarationStatement
	: TypeSpecifier VariableDeclarators ';'
	;

Statement
	: EmptyStatement
	| LabelStatement
	| ExpressionStatement ';'
        | SelectionStatement
        | IterationStatement
	| JumpStatement
	| Block
	;

EmptyStatement
	: ';'
        ;

LabelStatement
	: IDENTIFIER ':'
        | CASE ConstantExpression ':'
	| DEFAULT ':'
        ;

ExpressionStatement
	: Expression
	;

SelectionStatement
        : SWITCH '(' Expression ')' Block
        ;

IterationStatement
	: WHILE '(' Expression ')' Statement
	;

ForInit
	: ExpressionStatements ';'
	| LocalVariableDeclarationStatement
	| ';'
	;

ForExpr
	: Expression ';'
	| ';'
	;

ForIncr
	: ExpressionStatements
	;

ExpressionStatements
	: ExpressionStatement
	| ExpressionStatements ',' ExpressionStatement
	;

JumpStatement
	: BREAK IDENTIFIER ';'
	| BREAK            ';'
        | CONTINUE IDENTIFIER ';'
	| CONTINUE            ';'
	| RETURN Expression ';'
	| RETURN            ';'
	;


PrimaryExpression
	: QualifiedName
	| NotJustName
	;

NotJustName
	: SpecialName
	| NewAllocationExpression
	| ComplexPrimary
	;

ComplexPrimary
	: '(' Expression ')'
	| ComplexPrimaryNoParenthesis
	;

ComplexPrimaryNoParenthesis
	: LITERAL
	| ArrayAccess
	| FieldAccess
	| MethodCall
	;

ArrayAccess
	: QualifiedName '[' Expression ']'
	| ComplexPrimary '[' Expression ']'
	;

FieldAccess
	: NotJustName '.' IDENTIFIER
	| RealPostfixExpression '.' IDENTIFIER
        | QualifiedName '.' THIS
        | QualifiedName '.' CLASS
        | PrimitiveType '.' CLASS
	;

MethodCall
	: MethodAccess '(' ArgumentList ')'
	| MethodAccess '(' ')'
	;

MethodAccess
	: ComplexPrimaryNoParenthesis
	| SpecialName
	| QualifiedName
	;

SpecialName
	: THIS
	;

ArgumentList
	: Expression
	| ArgumentList ',' Expression
	;

NewAllocationExpression
        : PlainNewAllocationExpression
        | QualifiedName '.' PlainNewAllocationExpression
        ;

PlainNewAllocationExpression
    	: ArrayAllocationExpression
    	| ClassAllocationExpression
    	| ArrayAllocationExpression '{' '}'
    	| ClassAllocationExpression '{' '}'
    	| ArrayAllocationExpression '{' ArrayInitializers '}'
    	| ClassAllocationExpression '{' FieldDeclarations '}'
    	;

ClassAllocationExpression
	: NEW TypeName '(' ArgumentList ')'
	| NEW TypeName '('              ')'
        ;

ArrayAllocationExpression
	: NEW TypeName DimExprs Dims
	| NEW TypeName DimExprs
        | NEW TypeName Dims
	;

DimExprs
	: DimExpr
	| DimExprs DimExpr
	;

DimExpr
	: '[' Expression ']'
	;

Dims
	: OP_DIM
	| Dims OP_DIM
	;

PostfixExpression
	: PrimaryExpression
	| RealPostfixExpression
	;

RealPostfixExpression
	: PostfixExpression OP_INC
	| PostfixExpression OP_DEC
	;

UnaryExpression
	: OP_INC UnaryExpression
	| OP_DEC UnaryExpression
	| ArithmeticUnaryOperator CastExpression
	| LogicalUnaryExpression
	;

LogicalUnaryExpression
	: PostfixExpression
	| LogicalUnaryOperator UnaryExpression
	;

LogicalUnaryOperator
	: '~'
	| '!'
	;

ArithmeticUnaryOperator
	: '+'
	| '-'
	;

CastExpression
	: UnaryExpression
	| '(' PrimitiveTypeExpression ')' CastExpression
	| '(' ClassTypeExpression ')' CastExpression
	| '(' Expression ')' LogicalUnaryExpression
	;

PrimitiveTypeExpression
	: PrimitiveType
        | PrimitiveType Dims
        ;

ClassTypeExpression
	: QualifiedName Dims
        ;

MultiplicativeExpression
	: CastExpression
	| MultiplicativeExpression '*' CastExpression
	| MultiplicativeExpression '/' CastExpression
	| MultiplicativeExpression '%' CastExpression
	;

AdditiveExpression
	: MultiplicativeExpression
        | AdditiveExpression '+' MultiplicativeExpression
	| AdditiveExpression '-' MultiplicativeExpression
        ;

ShiftExpression
	: AdditiveExpression
        | ShiftExpression OP_SHL AdditiveExpression
        | ShiftExpression OP_SHR AdditiveExpression
        | ShiftExpression OP_SHRR AdditiveExpression
	;

RelationalExpression
	: ShiftExpression
        | RelationalExpression '<' ShiftExpression
	| RelationalExpression '>' ShiftExpression
	| RelationalExpression OP_LE ShiftExpression
	| RelationalExpression OP_GE ShiftExpression
	| RelationalExpression INSTANCEOF TypeSpecifier
	;

EqualityExpression
	: RelationalExpression
        | EqualityExpression OP_EQ RelationalExpression
        | EqualityExpression OP_NE RelationalExpression
        ;

AndExpression
	: EqualityExpression
        | AndExpression '&' EqualityExpression
        ;

ExclusiveOrExpression
	: AndExpression
	| ExclusiveOrExpression '^' AndExpression
	;

InclusiveOrExpression
	: ExclusiveOrExpression
	| InclusiveOrExpression '|' ExclusiveOrExpression
	;

ConditionalAndExpression
	: InclusiveOrExpression
	| ConditionalAndExpression OP_LAND InclusiveOrExpression
	;

ConditionalOrExpression
	: ConditionalAndExpression
	| ConditionalOrExpression OP_LOR ConditionalAndExpression
	;

ConditionalExpression
	: ConditionalOrExpression
	| ConditionalOrExpression '?' Expression ':' ConditionalExpression
	;

AssignmentExpression
	: ConditionalExpression
	| UnaryExpression AssignmentOperator AssignmentExpression
	;

AssignmentOperator
	: '='
	| ASS_MUL
	| ASS_DIV
	| ASS_MOD
	| ASS_ADD
	| ASS_SUB
	| ASS_SHL
	| ASS_SHR
	| ASS_SHRR
	| ASS_AND
	| ASS_XOR
	| ASS_OR
	;

Expression
	: AssignmentExpression
        ;

ConstantExpression
	: ConditionalExpression
	;


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
