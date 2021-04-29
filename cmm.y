%{
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/IRBuilder.h"

#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/ToolOutputFile.h"

#include <memory>
#include <algorithm>
#include <list>
#include <vector>
#include <utility>
#include <stack>

#include "list.h"
#include "symbol.h"
  
using namespace llvm;
using namespace std;

using parameter = pair<Type*,const char*>;
using parameter_list = std::list<parameter>;

stack<loop_info> loop_stack;
 
int num_errors;
int currLoopType, prevLoopType;
int NotSwitchLoop = 0;
int SwitchLoop = 1;

extern int yylex();   /* lexical analyzer generated from lex.l */

int yyerror(const char *error);
int parser_error(const char*);

void cmm_abort();
char *get_filename();
int get_lineno();

int loops_found=0;

extern Module *M;
extern LLVMContext TheContext;
 
Function *Fun;
IRBuilder<> *Builder;

Value* BuildFunction(Type* RetType, const char *name, 
			   parameter_list *params);

%}

/* Data structure for tree nodes*/

%union {
  int inum;
  char * id;
  Type*  type;
  Value* value;
  parameter_list *plist;
  vector<Value*> *arglist;
}

/* these tokens are simply their corresponding int values, more terminals*/

%token SEMICOLON COMMA MYEOF
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET

%token ASSIGN PLUS MINUS STAR DIV MOD 
%token LT GT LTE GTE EQ NEQ
%token BITWISE_OR BITWISE_XOR LSHIFT RSHIFT BITWISE_INVERT
%token DOT AMPERSAND 

%token FOR WHILE IF ELSE DO RETURN SWITCH
%token BREAK CONTINUE CASE DEFAULT COLON
%token INT VOID BOOL
%token I2P P2I SEXT ZEXT

/* NUMBER and ID have values associated with them returned from lex*/

%token <inum> CONSTANT_INTEGER /*data type of NUMBER is num union*/
%token <id>  ID

%left EQ NEQ LT GT LTE GTE
%left BITWISE_OR
%left BITWISE_XOR
%left AMPERSAND
%left LSHIFT RSHIFT
%left PLUS MINUS
%left MOD DIV STAR 
%nonassoc ELSE

%type <type> type_specifier
%type <value> opt_initializer statement
%type <value> expression bool_expression expr_opt assign_expression
%type <value> lvalue_location primary_expression unary_expression
%type <value> constant constant_expression unary_constant_expression
%type <arglist> argument_list_opt argument_list
%type <value> compound_stmt break_stmt return_stmt case_stmt 
%type <value> expr_stmt selection_stmt iteration_stmt continue_stmt

%type <plist> param_list param_list_opt

%%

translation_unit:	  external_declaration
			| translation_unit external_declaration
                        | translation_unit MYEOF
{
  YYACCEPT;
}
;

external_declaration:	  
  function_definition
  {
    //For debug case, keep large number of errors
    if(num_errors>500){
      cmm_abort();
    }
  }
| global_declaration { }
;

function_definition:	  
  type_specifier ID LPAREN param_list_opt RPAREN
  {
    symbol_push_scope();
    BuildFunction($1,$2,$4);
  }
  compound_stmt 
  {
    BasicBlock *BB = Builder->GetInsertBlock();
    if(!BB->getTerminator())
	  { 
      if(Fun->getFunctionType()->getReturnType()->isVoidTy())
			  Builder->CreateRetVoid();
		  else if(Fun->getFunctionType()->getReturnType()->isIntegerTy())
		    Builder->CreateRet(Builder->getInt64(0));
		  else if(Fun->getFunctionType()->getReturnType()->isPointerTy())			    
		    Builder->CreateRet(ConstantPointerNull::get(PointerType::get(Fun->getFunctionType()->getReturnType(),0)));			    
      else
  	    Builder->CreateRet(Builder->getInt64(0));
	  }
    symbol_pop_scope();
  }
| type_specifier STAR ID LPAREN param_list_opt RPAREN
  {
    symbol_push_scope();
    BuildFunction(PointerType::get($1,0),$3,$5);
  }
  compound_stmt
  {
    BasicBlock *BB = Builder->GetInsertBlock();
    if(!BB->getTerminator())
	  { 
      if(Fun->getFunctionType()->getReturnType()->isVoidTy())
			  Builder->CreateRetVoid();
		  else if(Fun->getFunctionType()->getReturnType()->isIntegerTy())
		    Builder->CreateRet(Builder->getInt64(0));
		  else if(Fun->getFunctionType()->getReturnType()->isPointerTy())			    
		    Builder->CreateRet(ConstantPointerNull::get(PointerType::get(Fun->getFunctionType()->getReturnType(),0)));			    
      else
  	    Builder->CreateRet(Builder->getInt64(0));
	  }
    symbol_pop_scope();
  }
;

global_declaration:    
  type_specifier STAR ID opt_initializer SEMICOLON
  {
    if(is_global_scope()) {
		  Twine name($3);
		  new GlobalVariable(*M,(Type*)PointerType::get($1,0),false,GlobalValue::ExternalLinkage,(Constant*)NULL,name);
	  }
  }  
  | type_specifier ID opt_initializer SEMICOLON
  {
    if (is_global_scope()) {
	    Twine name($2);
	    new GlobalVariable(*M,(Type*)$1,false,GlobalValue::ExternalLinkage,(Constant*)NULL,name);
	  }
  }
;

opt_initializer: 
  ASSIGN constant_expression 
  { 
    $$ = $2; 
  } 
  | 
  { 
    $$ = nullptr; 
  } 
;

type_specifier:		  INT
{
  $$ = Type::getInt64Ty(TheContext);
}
|    VOID
{
  $$ = Type::getVoidTy(TheContext);
}
;

param_list_opt:           
{
  $$ = nullptr;
}
| param_list
{
  $$ = $1;
}
;

param_list:	
param_list COMMA type_specifier ID
{
  $$ = $1;
  $$->push_back( parameter($3,$4) );
}
| param_list COMMA type_specifier STAR ID
{
  $$ = $1;
  $$->push_back( parameter(PointerType::get($3,0),$5) );
}
| type_specifier ID
{
  $$ = new parameter_list;
  $$->push_back( parameter($1,$2) );
}
| type_specifier STAR ID
{
  $$ = new parameter_list;
  $$->push_back( parameter(PointerType::get($1,0),$3) );
}
;

statement:		  
expr_stmt { $$ = $1; }           
			| compound_stmt     { $$ = $1; }       
			| selection_stmt   { $$ = $1; }        
			| iteration_stmt   { $$ = $1; }        
			| return_stmt   { $$ = $1; }             
      | break_stmt { $$ = $1; }   
      | continue_stmt { $$ = $1; }   
      | case_stmt { $$ = $1; }   
;

expr_stmt:	           
  SEMICOLON 
  { 

  }           
| assign_expression SEMICOLON  
  { 
    $$ = $1;
  }    
;

local_declaration:    
  type_specifier STAR ID opt_initializer SEMICOLON
  {
    Value * ai = Builder->CreateAlloca(PointerType::get($1,0),Builder->getInt64(100),$3);
    if (nullptr != $4)
      Builder->CreateStore($4,ai);
    symbol_insert($3,ai);
  }
| type_specifier ID opt_initializer SEMICOLON
  {
    Value * ai = Builder->CreateAlloca($1,0,$2);
    if (nullptr != $3)
      Builder->CreateStore($3,ai);
    symbol_insert($2,ai);  
  }
;

local_declaration_list:	   
   local_declaration { }
 | local_declaration_list local_declaration  { }
;

local_declaration_list_opt:	 { }
			| local_declaration_list { }
;

compound_stmt:		  LBRACE {
  // PUSH SCOPE TO RECORD VARIABLES WITHIN COMPOUND STATEMENT
  symbol_push_scope();
}
local_declaration_list_opt 
{

}
statement_list_opt 
{
  // POP SCOPE TO REMOVE VARIABLES NO LONGER ACCESSIBLE
  symbol_pop_scope();
}
RBRACE { }
;

statement_list_opt:	{ }
			| statement_list { }
;

statement_list:		statement { }
		      | statement_list statement { }
;

break_stmt:               
  BREAK SEMICOLON 
  {
    if (prevLoopType == NotSwitchLoop) {
      loop_info_t info = get_loop();
		  if(info.exit) {
		    Builder->CreateBr(info.exit);
      }
		  else {
		    parser_error("BREAK not detected inside a loop\n");
      }
    }
    if (prevLoopType == SwitchLoop) {
      switch_info_t info = get_switch();
		  if(info.defaultCase) {
        pop_switch();
				push_switch(info.defaultCase, info.body, info.lastCase, 1);
		    Builder->CreateBr(info.defaultCase);
      }
		  else {
		    parser_error("BREAK not detected inside a switch\n");
      }
    }
  }

;

case_stmt:                
  CASE constant_expression COLON
  {
    switch_info_t info = get_switch();
    if(info.defaultCase!=NULL) {
      BasicBlock *blockk = BasicBlock::Create(M->getContext(), "switch:case", Fun);
		  if(info.broken!=1) {
        Builder->CreateBr(blockk); 
	    }
	    pop_switch();
      push_switch(info.defaultCase, info.body, blockk, 0);
      info.body->addCase((dyn_cast<ConstantInt>)$2, blockk);    
      Builder->SetInsertPoint(blockk);
    }
  }
;

continue_stmt:            
  CONTINUE SEMICOLON
  {
    loop_info_t info = get_loop();
		if(info.exit) {
		  Builder->CreateBr(info.expr);
    }
		else {
		  parser_error("CONTINUE not detected inside a loop\n");
    }
  }
;

selection_stmt:		  
  IF LPAREN bool_expression RPAREN 
  {
    BasicBlock *then = BasicBlock::Create(M->getContext(), "if:then", Fun);
		BasicBlock *els =  BasicBlock::Create(M->getContext(), "if:else", Fun);
		BasicBlock *join = BasicBlock::Create(M->getContext(), "if:join", Fun);
		if($3->getType() == Builder->getInt1Ty())
    {
			Builder->CreateCondBr($3, then, els);			
		}
		else if($3->getType() == Builder->getInt64Ty())
		{
			Builder->CreateCondBr(Builder->CreateICmpNE($3, Builder->getInt64(0)), then, els);
		}
    push_if(then, els, join);
    Builder->SetInsertPoint(then);
  }
  statement 
  {
    if_info_t info = get_if();
		if(info.join == NULL) {
			parser_error("Loop_info_t info is not initialized\n");
		}
    if(!Builder->GetInsertBlock()->getTerminator())
		  Builder->CreateBr(info.join);
		Builder->SetInsertPoint(info.els);
  }
  ELSE statement
  {
    if_info_t info = get_if();
    pop_if();
    if(!Builder->GetInsertBlock()->getTerminator())
		  Builder->CreateBr(info.join);
		Builder->SetInsertPoint(info.join);

  }
| SWITCH LPAREN expression RPAREN 
  {
    BasicBlock *defaultCase = BasicBlock::Create(M->getContext(), "switch:default", Fun);
	  SwitchInst *body = Builder->CreateSwitch($3, defaultCase, 10);
	  push_switch(defaultCase, body, NULL, 1);
    currLoopType = prevLoopType;
	  prevLoopType = SwitchLoop;
  }
  statement 
  {
    switch_info_t info = get_switch();
		pop_switch();
    prevLoopType = currLoopType;
    BasicBlock *blockk = Builder->GetInsertBlock();
	  if(!blockk->getTerminator())
	    Builder->CreateBr(info.defaultCase);
	  Builder->SetInsertPoint(info.defaultCase);
  }
;

iteration_stmt:
  WHILE 
  {
    BasicBlock *expr = BasicBlock::Create(M->getContext(), "while:expr", Fun);
	  Builder->CreateBr(expr);
	  Builder->SetInsertPoint(expr);
    push_loop(expr,expr,expr,expr);
  }
  LPAREN bool_expression RPAREN 
  {
    loop_info_t info = get_loop();
	  pop_loop();
    BasicBlock *body = BasicBlock::Create(M->getContext(), "while:body", Fun); 
	  BasicBlock *exit = BasicBlock::Create(M->getContext(), "while:exit", Fun); 
	  push_loop(info.expr,body,body,exit);
	  Builder->CreateCondBr($4,body,exit);
	  Builder->SetInsertPoint(body);  
 	  currLoopType = prevLoopType;
	  prevLoopType = NotSwitchLoop;
  }
  statement
  {
 	  loop_info_t info = get_loop();
	  Builder->CreateBr(info.expr);
	  Builder->SetInsertPoint(info.exit);
	  pop_loop();  
	  prevLoopType = currLoopType;
  }
| FOR LPAREN expr_opt SEMICOLON 
  {
	  BasicBlock *expr = BasicBlock::Create(M->getContext(), "for:expr", Fun);
	  Builder->CreateBr(expr);
	  Builder->SetInsertPoint(expr);
    push_loop(expr,NULL,NULL,NULL);
	}
  bool_expression SEMICOLON expr_opt RPAREN
  {
    loop_info_t info = get_loop();
	  pop_loop();
    BasicBlock *body = BasicBlock::Create(M->getContext(), "for:body", Fun); 
	  BasicBlock *exit = BasicBlock::Create(M->getContext(), "for:exit", Fun); 
	  push_loop(info.expr,body,body,exit);
	  Builder->CreateCondBr($6,body,exit);
	  Builder->SetInsertPoint(body);  
 	  currLoopType = prevLoopType;
	  prevLoopType = NotSwitchLoop;
  }
  statement 
	{
	  loop_info_t info = get_loop();
	  Builder->CreateBr(info.expr);
	  Builder->SetInsertPoint(info.exit);
	  pop_loop();  
	  prevLoopType = currLoopType;
  }
| DO 
  {
		BasicBlock *expr = BasicBlock::Create(M->getContext(), "do:expr", Fun);
    BasicBlock *body = BasicBlock::Create(M->getContext(), "do:body", Fun);
		BasicBlock *exit = BasicBlock::Create(M->getContext(), "do:exit", Fun);
    push_loop(expr,body,NULL,exit);
    Builder->CreateBr(body);
    Builder->SetInsertPoint(body);
    currLoopType = prevLoopType;
	  prevLoopType = NotSwitchLoop;
  }
  statement WHILE 
  {
	  loop_info_t loop = get_loop();
	  Builder->CreateBr(loop.expr);
	  Builder->SetInsertPoint(loop.expr);
  } 
  LPAREN bool_expression RPAREN SEMICOLON
  {
    loop_info_t my_loop = get_loop();
		pop_loop();
    Builder->CreateCondBr($7, my_loop.body, my_loop.exit);
		Builder->SetInsertPoint(my_loop.exit);
 		prevLoopType = currLoopType;
	}
;

expr_opt:  	
  {
    $$ = nullptr;
  }
	| assign_expression 
  {
    $$ = $1;
  }
;

return_stmt:		  
  RETURN SEMICOLON 
  {
    Builder->CreateRetVoid();
  }
| RETURN expression SEMICOLON
  {
    
    if(Fun->getFunctionType()->getReturnType() == $2->getType()) {
      Builder->CreateRet($2);
    }
    else {
      if($2->getType() == Builder->getInt64Ty()) {
        if(Fun->getFunctionType()->getReturnType() == Builder->getInt1Ty()) {
				  Value *value = Builder->CreateICmpNE($2,Builder->getInt64(0));
				  Builder->CreateRet(value);
				}
      }
      else if($2->getType() == Builder->getInt1Ty()) {
        if(Fun->getFunctionType()->getReturnType() == Builder->getInt64Ty()) {
			    Value *value = Builder->CreateZExt($2,Builder->getInt64Ty());
			    Builder->CreateRet(value);
			  }
      }
    }
  }
;

bool_expression: 
  expression 
  {
    $$ = Builder->CreateICmpNE($1,Builder->getInt64(0));
  }
;

assign_expression:
  lvalue_location ASSIGN expression
  {
    Builder->CreateStore($3,$1);
  }
| expression 
  {
    $$ = $1;
  }
;

expression:
  unary_expression
  {
    $$ = $1;
  }
| expression BITWISE_OR expression
  {
    $$ = Builder->CreateOr($1,$3);
  }
| expression BITWISE_XOR expression
  {
    $$ = Builder->CreateXor($1,$3);
  }
| expression AMPERSAND expression
  {
    $$ = Builder->CreateAnd($1,$3);
  }
| expression EQ expression
  {
    Value *value = Builder->CreateICmpEQ($1,$3);
    $$ = Builder->CreateSelect(value, Builder->getInt64(1), Builder->getInt64(0));
  }
| expression NEQ expression
  {
    Value *value = Builder->CreateICmpNE($1,$3);
    $$ = Builder->CreateSelect(value, Builder->getInt64(1), Builder->getInt64(0));
    //$$ = Builder->CreateZExt(value, Builder->getInt64Ty());
  }
| expression LT expression 
  {
    Value *value = Builder->CreateICmpSLT($1,$3);
    $$ = Builder->CreateSelect(value, Builder->getInt64(1), Builder->getInt64(0));
  }
| expression GT expression
  {
    Value *value = Builder->CreateICmpSGT($1,$3);
    $$ = Builder->CreateSelect(value, Builder->getInt64(1), Builder->getInt64(0));
  }
| expression LTE expression
  {
    Value *value = Builder->CreateICmpSLE($1,$3);
    $$ = Builder->CreateSelect(value, Builder->getInt64(1), Builder->getInt64(0));
  }
| expression GTE expression
  {
    Value *value = Builder->CreateICmpSGE($1,$3);
    $$ = Builder->CreateSelect(value, Builder->getInt64(1), Builder->getInt64(0));
  }
| expression LSHIFT expression
  {
    $$ = Builder->CreateShl($1,$3);
  }
| expression RSHIFT expression
  {
    $$ = Builder->CreateLShr($1,$3);
  }
| expression PLUS expression
  {
    $$ = Builder->CreateAdd($1,$3);
  }
| expression MINUS expression
  {
    $$ = Builder->CreateSub($1,$3);
  }
| expression STAR expression
  {
    $$ = Builder->CreateMul($1,$3);
  }
| expression DIV expression
  {
    $$ = Builder->CreateSDiv($1,$3);
  }
| expression MOD expression
  {
    $$ = Builder->CreateSRem($1,$3);
  }
| BOOL LPAREN expression RPAREN
{
  Value *value = Builder->CreateICmpNE($3,Builder->getInt64(0));
  $$ = Builder->CreateSelect(value, Builder->getInt64(1), Builder->getInt64(0));
}
| I2P LPAREN expression RPAREN
{
  if($3->getType() == Builder->getInt64Ty()) {
    $$ = Builder->CreateIntToPtr($3,PointerType::get(Builder->getInt64Ty(),0),"");
  }
}
| P2I LPAREN expression RPAREN
{
  if(PointerType *py = dyn_cast<PointerType>($3->getType())) {
    $$ = new PtrToIntInst($3,Builder->getInt64Ty()); 
  }
}
| ZEXT LPAREN expression RPAREN
  {
    Value *value = Builder->CreateZExt($3,Builder->getInt64Ty());
    $$ = value;
  }
| SEXT LPAREN expression RPAREN
  { 
    Value *value = Builder->CreateSExt($3,Builder->getInt64Ty());
    $$ = value;
  }
| ID LPAREN argument_list_opt RPAREN 
  {
    Value* value = symbol_find($1);
    $$ = Builder->CreateLoad(value);
  }
| LPAREN expression RPAREN
  {
    $$ = $2;
  }
;

argument_list_opt: { }
| argument_list
  {
    $$ = $1;
  }
;

argument_list:
  expression 
  {
    $$ = new vector<Value*>();
    $$->push_back($1);
  }
| argument_list COMMA expression 
  {
    $$ = $1;
    $$->push_back($3);
  }
;

unary_expression:         
  primary_expression
  {
    $$ = $1;
  }
| AMPERSAND primary_expression
  {
    if((dyn_cast<Instruction>$2)->hasAtomicLoad()) {  
			Value *value = (dyn_cast<Instruction>$2)->getOperand(0);
      $$ = value;
      (dyn_cast<Instruction>$2)->eraseFromParent();
    }
  }
| STAR primary_expression 
  {
    if($2->getType()->isPointerTy()) {   
		  Value *value = Builder->CreateLoad($2);
		  $$ = value;
		}          
		else {
			parser_error("*e Expression isn't a pointer \n");
		}
  }
| MINUS unary_expression 
  {
    $$ = Builder->CreateNeg($2);
  }
| PLUS unary_expression
  {
    $$ = $2;
  }
| BITWISE_INVERT unary_expression
  {
    if($2->getType() == Builder->getInt64Ty()) 
    {
			$$ = Builder->CreateXor($2, llvm::ConstantInt::get(Builder->getInt64Ty(), 0xFFFFFFFF), "");
    }
    else
		{
			parser_error("Invalid operands for ~ operation on UE");
		}
  }
;

primary_expression:
  lvalue_location 
  {
    $$ = Builder->CreateLoad($1);
  }
| constant 
  {
    $$ = $1;
  }
;

lvalue_location:
  ID 
  {
    Value* value = symbol_find($1);
		if (value!=NULL) {
      $$ = value;
		} 
		else {
 		  symbol_push_scope();
      Value *value = Builder->CreateAlloca(Builder->getInt64Ty(),nullptr,$1);
      $$ = value;
    }
  }
| lvalue_location LBRACKET expression RBRACKET 
  {
    if((dyn_cast<Instruction>$3)->hasAtomicLoad()) {  
			Value *value = (dyn_cast<Instruction>$3)->getOperand(0);
      $$ = Builder->CreateLoad(value);
      (dyn_cast<Instruction>$3)->eraseFromParent();
    }
    //Value *ptr = Builder->CreateGEP($1, $3, "gep");
    //$$ = Builder->CreateLoad($3);
  }
| STAR LPAREN expression RPAREN 
  {
    if($3->getType()->isPointerTy()) {   
			Value *value = Builder->CreateLoad($3);
			$$ = $3;
		}          
		else {
			parser_error("*(e) Expression is not a pointer\n");
		}
  }
;

constant_expression:
  unary_constant_expression
  {
    $$ = $1;
  }
| constant_expression BITWISE_OR constant_expression
  {
    if(($1->getType() == Builder->getInt64Ty()) && ($3->getType() == Builder->getInt64Ty())) {
			$$ = Builder->CreateOr($1,$3);
    }
		else {
			parser_error("Invalid operands for OR operation\n");
    }
  }
| constant_expression BITWISE_XOR constant_expression 
  {
    if(($1->getType() == Builder->getInt64Ty()) && ($3->getType() == Builder->getInt64Ty())) {
			$$ = Builder->CreateXor($1,$3);
    }
		else {
			parser_error("Invalid operands for XOR operation\n");
    }
  }
| constant_expression AMPERSAND constant_expression
  {
    if(($1->getType() == Builder->getInt64Ty()) && ($3->getType() == Builder->getInt64Ty())) {
			$$ = Builder->CreateAnd($1,$3);
    }
		else {
			parser_error("Invalid operands for AND operation\n");
    }
  }
| constant_expression LSHIFT constant_expression
  {
    if(($1->getType() == Builder->getInt64Ty()) && ($3->getType() == Builder->getInt64Ty())) {
			$$ = Builder->CreateShl($1,$3);
    }
		else {
			parser_error("Invalid operands for LSHIFT operation\n");
    }
  }
| constant_expression RSHIFT constant_expression
  {
    if(($1->getType() == Builder->getInt64Ty()) && ($3->getType() == Builder->getInt64Ty())) {
			$$ = Builder->CreateLShr($1,$3);
    }
		else {
			parser_error("Invalid operand for RSHIFT operation\n");
    }
  }
| constant_expression PLUS constant_expression 
  {
    if(($1->getType() == Builder->getInt64Ty()) && ($3->getType() == Builder->getInt64Ty())) {
			$$ = Builder->CreateAdd($1,$3);
    }
		else {
			parser_error("Invalid operand for PLUS operation\n");
    }
  }
| constant_expression MINUS constant_expression
  {
    if(($1->getType() == Builder->getInt64Ty()) && ($3->getType() == Builder->getInt64Ty())) {
			$$ = Builder->CreateSub($1,$3);
    }
		else {
			parser_error("Invalid operand for MINUS operation\n");
    }
  }
| constant_expression STAR constant_expression
  {
    if(($1->getType() == Builder->getInt64Ty()) && ($3->getType() == Builder->getInt64Ty())) {
			$$ = Builder->CreateMul($1,$3);
    }
		else {
			parser_error("Invalid operand for MUL operation\n");
    }
  }
| constant_expression DIV constant_expression
  {
    if(($1->getType() == Builder->getInt64Ty()) && ($3->getType() == Builder->getInt64Ty())) {
			$$ = Builder->CreateSDiv($1,$3);
    }
		else {
			parser_error("Invalid operand for DIV operation\n");
    }
  }
| constant_expression MOD constant_expression
  {
    if(($1->getType() == Builder->getInt64Ty()) && ($3->getType() == Builder->getInt64Ty())) {
			$$ = Builder->CreateSRem($1,$3);
    }
		else {
			parser_error("Invalid operand for MOD operation\n");
    }
  }
| I2P LPAREN constant_expression RPAREN 
  {
    if(($3->getType() == Builder->getInt64Ty())) {
      $$ = Builder->CreateIntToPtr($3,PointerType::get(Builder->getInt64Ty(),0),"");
    }
    else {
      parser_error("Expression is not int type\n");
    }
  }
| LPAREN constant_expression RPAREN
  {
    $$ = $2;
  }
;

unary_constant_expression:
  constant 
  {
    $$ = $1;
  }
| MINUS unary_constant_expression
  {
    $$ = Builder->CreateNeg($2);
  }
| PLUS unary_constant_expression
  {
    $$ = $2;
  }
| BITWISE_INVERT unary_constant_expression
  {
		$$ = Builder->CreateXor($2, llvm::ConstantInt::get(Builder->getInt64Ty(), 0xFFFFFFFF), "");
  }
;

constant:	 
  CONSTANT_INTEGER
  {
    $$ = Builder->getInt64($1);
  }
;


%%

Value* BuildFunction(Type* RetType, const char *name, 
			   parameter_list *params)
{
  std::vector<Type*> v;
  std::vector<const char*> vname;

  if (params)
    for(auto ii : *params)
      {
	vname.push_back( ii.second );
	v.push_back( ii.first );      
      }
  
  ArrayRef<Type*> Params(v);

  FunctionType* FunType = FunctionType::get(RetType,Params,false);

  Fun = Function::Create(FunType,GlobalValue::ExternalLinkage,
			 name,M);
  Twine T("entry");
  BasicBlock *BB = BasicBlock::Create(M->getContext(),T,Fun);

  /* Create an Instruction Builder */
  Builder = new IRBuilder<>(M->getContext());
  Builder->SetInsertPoint(BB);

  Function::arg_iterator I = Fun->arg_begin();
  for(int i=0; I!=Fun->arg_end();i++, I++)
    {
      // map args and create allocas!
      AllocaInst *AI = Builder->CreateAlloca(v[i]);
      Builder->CreateStore(&(*I),(Value*)AI);
      symbol_insert(vname[i],(Value*)AI);
    }


  return Fun;
}

extern int verbose;
extern int line_num;
extern char *infile[];
static int   infile_cnt=0;
extern FILE * yyin;
extern int use_stdin;

int parser_error(const char *msg)
{
  if (use_stdin)
    printf("stdin:%d: Error -- %s\n",line_num,msg);
  else
    printf("%s:%d: Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int internal_error(const char *msg)
{
  printf("%s:%d Internal Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int yywrap() {

  if (use_stdin)
    {
      yyin = stdin;
      return 0;
    }
  
  static FILE * currentFile = NULL;

  if ( (currentFile != 0) ) {
    fclose(yyin);
  }
  
  if(infile[infile_cnt]==NULL)
    return 1;

  currentFile = fopen(infile[infile_cnt],"r");
  if(currentFile!=NULL)
    yyin = currentFile;
  else
    printf("Could not open file: %s",infile[infile_cnt]);

  infile_cnt++;
  
  return (currentFile)?0:1;
}

int yyerror(const char* error)
{
  parser_error("Un-resolved syntax error.");
  return 1;
}

char * get_filename()
{
  return infile[infile_cnt-1];
}

int get_lineno()
{
  return line_num;
}


void cmm_abort()
{
  parser_error("Too many errors to continue.");
  exit(1);
}
