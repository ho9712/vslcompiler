#ifndef __PARSER_H__
#define __PARSER_H__
#include"lexer.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include <cassert>
#include <cstdint>
#include <utility>
#include "../include/KaleidoscopeJIT.h"


using namespace llvm;
using namespace llvm::orc;
//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//
namespace {
	//表达式抽象语法树基类
	class ExprAST {
	public:
		virtual ~ExprAST() = default;
		virtual Value *codegen() = 0;
	};


	//statement抽象语法树基类
	class StatAST {
	public:
		virtual ~StatAST() = default;
		virtual Value* codegen() = 0;
	};


	//数字抽象语法树
	class NumberExprAST : public StatAST {
		int Val;

	public:
		NumberExprAST(int Val) : Val(Val) {}

		Value * codegen() override;
	};

	//变量抽象语法树
	class VariableExprAST : public StatAST {
		std::string Name;

	public:
		std::string getName() {
			return Name;
		}

		VariableExprAST(const std::string &Name) : Name(Name) {}

		Value * codegen() override;
	};



	//二元运算表达式抽象语法树
	class BinaryExprAST : public StatAST {
		char Op;
		std::unique_ptr<StatAST> LHS, RHS;

	public:
		BinaryExprAST(char Op, std::unique_ptr<StatAST> LHS,
			std::unique_ptr<StatAST> RHS)
			: Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}


		Value * codegen() override;
	};

	//函数原型抽象语法树--函数名和参数列表
	class PrototypeAST {
		std::string Name;
		std::vector<std::string> Args;

	public:
		PrototypeAST(const std::string &Name, std::vector<std::string> Args)
			: Name(Name), Args(std::move(Args)) {}

		const std::string &getName() const { return Name; }

		Function * codegen();
	};




	//变量声明 语法树
	class DecStatAST : public StatAST {
		std::vector<std::string> VarNames;
		std::unique_ptr<StatAST> Body;

	public:
		DecStatAST(std::vector<std::string> VarNames, std::unique_ptr<StatAST> Body)
			:VarNames(std::move(VarNames)), Body(std::move(Body)) {}

		Value *codegen() override;
	};

	//block 函数快语法树
	class BlockStatAST : public StatAST {
		std::vector<std::unique_ptr<StatAST>> DecList;
		std::vector<std::unique_ptr<StatAST>> StatList;

	public:
		BlockStatAST(std::vector<std::unique_ptr<StatAST>> DecList, std::vector<std::unique_ptr<StatAST>> StatList)
			:DecList(std::move(DecList)), StatList(std::move(StatList)) {}

	public:
		Value *codegen() override;
	};

	

	//IF 语法树
	class IfStatAST : public StatAST {
		std::unique_ptr<StatAST> Cond;
		std::unique_ptr<StatAST> Then, Else;

	public:
		IfStatAST(std::unique_ptr<StatAST> Cond, std::unique_ptr<StatAST> Then,
			std::unique_ptr<StatAST> Else)
			: Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

		Value *codegen() override;
	};

    //RETURN　语法树
	class RetStatAST : public StatAST {
		std::unique_ptr<StatAST> Val;

	public:
		RetStatAST(std::unique_ptr<StatAST> Val)
			: Val(std::move(Val)) {}

		Value *codegen() override;
	};

	//Assignment 语法树
	class AssStatAST : public StatAST {
		std::unique_ptr<VariableExprAST> Name;
		std::unique_ptr<StatAST> Expression;

	public:
		AssStatAST(std::unique_ptr<VariableExprAST> Name, std::unique_ptr<StatAST> Expression)
			: Name(std::move(Name)), Expression(std::move(Expression)) {}

		Value *codegen() override;
	};

	//函数抽象语法树
	class FunctionAST {
		std::unique_ptr<PrototypeAST> Proto;
		std::unique_ptr<StatAST> Body;

	public:
		FunctionAST(std::unique_ptr<PrototypeAST> Proto,
			std::unique_ptr<StatAST> Body)
			: Proto(std::move(Proto)), Body(std::move(Body)) {}

		Function * codegen();
	};

	//函数调用抽象语法树
	class CallExprAST : public StatAST {
		std::string Callee;
		std::vector<std::unique_ptr<StatAST>> Args;
	public:
		CallExprAST(const std::string &Callee,
			std::vector<std::unique_ptr<StatAST>> Args)
			: Callee(Callee), Args(std::move(Args)) {}

		Value * codegen() override;
	};


	//程序的抽象语法树
	class ProgramAST {
		std::vector<std::unique_ptr<FunctionAST>> funcs;

	public:
		ProgramAST(std::vector<std::unique_ptr<FunctionAST>> funcs)
			:funcs(std::move(funcs)) {}
	};
	//while
	class WhileStatAST :public StatAST {
		std::unique_ptr<StatAST> While, Do;
	public:
		WhileStatAST(std::unique_ptr<StatAST> While, std::unique_ptr<StatAST> Do) :
			While(std::move(While)), Do(std::move(Do)) {}

		Value *codegen() override;
	};

	std::unique_ptr<StatAST> LogError(const char *Str) {
		fprintf(stderr, "Error: %s\n", Str);
		return nullptr;
	}

	Value *LogErrorV(const char *Str) {
		LogError(Str);
		return nullptr;
	}

	
} // end anonymous namespace


//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

//运算符优先级
static std::map<char, int> BinopPrecedence;

///获得运算符优先级
static int GetTokPrecedence() {
	if (!isascii(CurTok))
		return -1;

	// 确保运算符
	int TokPrec = BinopPrecedence[CurTok];
	if (TokPrec <= 0)
		return -1;
	return TokPrec;
}

//错误处理
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
	LogError(Str);
	return nullptr;
}
std::unique_ptr<FunctionAST> LogErrorF(const char *Str) {
	LogError(Str);
	return nullptr;
}
std::unique_ptr<DecStatAST> LogErrorD(const char *Str) {
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}

static std::unique_ptr<StatAST> ParseExpression();
static std::unique_ptr<StatAST> ParseIfStat();
static std::unique_ptr<StatAST> ParseWhileStat();
static std::unique_ptr<StatAST> ParseRetStat();
static std::unique_ptr<StatAST> ParseDecStat();
static std::unique_ptr<StatAST> ParseAssStat();
static std::unique_ptr<StatAST> ParseBlock();

//解析数字
static std::unique_ptr<StatAST> ParseNumberExpr() {
	auto Result = llvm::make_unique<NumberExprAST>(NumVal);
	getNextToken(); // consume the number
	return std::move(Result);
}

//解析括号
static std::unique_ptr<StatAST> ParseParenExpr() {
	getNextToken(); // eat (.
	auto V = ParseExpression();
	if (!V)
		return nullptr;

	if (CurTok != ')')
		return LogError("expected ')'");
	getNextToken(); // eat ).
	return V;
}

/// 解析表达式
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<StatAST> ParseIdentifierExpr() {
	std::string IdName = IdentifierStr;

	getNextToken();

	//解析成变量表达式
	if (CurTok != '(')
		return llvm::make_unique<VariableExprAST>(IdName);

	// 解析成函数调用表达式
	getNextToken();
	std::vector<std::unique_ptr<StatAST>> Args;
	if (CurTok != ')') {
		while (true) {
			if (auto Arg = ParseExpression())
				Args.push_back(std::move(Arg));
			else
				return nullptr;

			if (CurTok == ')')
				break;

			if (CurTok != ',')
				return LogError("Expected ')' or ',' in argument list");
			getNextToken();
		}
	}

	getNextToken();

	return llvm::make_unique<CallExprAST>(IdName, std::move(Args));
}

//解析字符种类：标识符 整数 括号
static std::unique_ptr<StatAST> ParsePrimary() {
	switch (CurTok) {
	default:
		return LogError("unknown token when expecting an expression");
	case VARIABLE:
		return ParseIdentifierExpr();
	case INTEGER:
		return ParseNumberExpr();
	case '(':
		return ParseParenExpr();
	case IF:
		return ParseIfStat();
		break;
	case RETURN:
		return ParseRetStat();
	case VAR:
		return ParseDecStat();
		break;
	case WHILE:
		return ParseWhileStat();
		break;
	}
}

//statment类型
static std::unique_ptr<StatAST> ParseStatement()
{
	switch (CurTok) {
	case'{':
		return ParseBlock();
	case IF:
		return ParseIfStat();
		break;
	case RETURN:
		return ParseRetStat();
	case VAR:
		return ParseDecStat();
		break;
	case WHILE:
		return ParseWhileStat();
		break;
	default:
		return ParseAssStat();
	}
}
//解析函数块
//block::='{' declaration_list statement_list '}'
static std::unique_ptr<StatAST> ParseBlock() {
	std::vector<std::unique_ptr<StatAST>> DecList;
	std::vector<std::unique_ptr<StatAST>> StatList;
	getNextToken();   //eat '{'
	if (CurTok == VAR) {
		auto varDec = ParseDecStat();
		DecList.push_back(std::move(varDec));
	}
	while (CurTok != '}') {
        if (CurTok == '{') {
			ParseBlock();
		}
		else if (CurTok == CONTINUE) {
			getNextToken();
		}
		else {
			auto statResult = ParseStatement();
			StatList.push_back(std::move(statResult));
		}
	}
	getNextToken();  //eat '}'

	return llvm::make_unique<BlockStatAST>(std::move(DecList), std::move(StatList));
}

///解析带运算符的表达式
///   ::= ('+' primary)*
static std::unique_ptr<StatAST> ParseBinOpRHS(int ExprPrec,
	std::unique_ptr<StatAST> LHS) {
	// 查询是否存在运算符及优先级
	while (true) {
		int TokPrec = GetTokPrecedence();

		//判断是否右部存在运算符和运算符优先级
		if (TokPrec < ExprPrec)
			return LHS;

		// 当右部没有运算符或右部运算符优先级小于左部运算符优先级时 退出循环和递归
		if (TokPrec < ExprPrec)
			return LHS;

		if (CurTok == ';')
			return LHS;

		// Okay, we know this is a binop.
		int BinOp = CurTok;
		getNextToken(); // eat binop

						// Parse the primary expression after the binary operator.
		auto RHS = ParsePrimary();
		if (!RHS)
			return nullptr;

		// If BinOp binds less tightly with RHS than the operator after RHS, let
		// the pending operator take RHS as its LHS.
		int NextPrec = GetTokPrecedence();
		if (TokPrec < NextPrec) {
			RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
			if (!RHS)
				return nullptr;
		}

		// 重新生成左部
		LHS = llvm::make_unique<BinaryExprAST>(BinOp, std::move(LHS),
			std::move(RHS));
	}
}

/// 解析表达式
///   ::= primary binoprhs
///
static std::unique_ptr<StatAST> ParseExpression() {
	auto LHS = ParsePrimary();
	if (!LHS)
		return nullptr;
	return ParseBinOpRHS(0, std::move(LHS));
}

///函数原型
///   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
	if (CurTok != VARIABLE)
		return LogErrorP("Expected function name in prototype");

	std::string FnName = IdentifierStr;
	getNextToken();

	if (CurTok != '(')
		return LogErrorP("Expected '(' in prototype");
	

	std::vector<std::string> ArgNames;
	getNextToken();
	while (CurTok == VARIABLE)
	{
		ArgNames.push_back(IdentifierStr);
		getNextToken();
		if (CurTok == ',')
			getNextToken();
	}
	if (CurTok != ')')
		return LogErrorP("Expected ')' in prototype");


	// success.
	getNextToken(); // eat ')'.

	return llvm::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

//解析函数 ::= FUNC prototype '{' statement '}'
static std::unique_ptr<FunctionAST> ParseFunc()
{
	getNextToken(); // eat FUNC.
	auto Proto = ParsePrototype();
	if (!Proto)
		return nullptr;
	auto E = ParseStatement();
	if (!E)
		return nullptr;

	return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
	if (auto E = ParseExpression()) {
		// Make an anonymous proto.
		auto Proto = llvm::make_unique<PrototypeAST>("__anon_expr",
			std::vector<std::string>());
		return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	}
	return nullptr;
}

//解析 if语句
static std::unique_ptr<StatAST> ParseIfStat() {
	getNextToken();  // eat the if.

	// condition.
	auto Cond = ParseExpression();
	if (!Cond)
		return nullptr;

	if (CurTok != THEN)
		return LogError("expected then");
	getNextToken();  // eat the then

	auto Then = ParseExpression();
	if (!Then)
		return nullptr;

	std::unique_ptr<StatAST> Else = nullptr;
	if (CurTok == ELSE) {
		getNextToken();
		Else = ParseExpression();
		if (!Else)
			return nullptr;
	}
	else if (CurTok != FI)
		return LogError("expected FI or ELSE");

	if (CurTok != FI)
		return LogError("expected FI");

	getNextToken();
	return llvm::make_unique<IfStatAST>(std::move(Cond), std::move(Then),
		std::move(Else));
}

//解析 while
static std::unique_ptr<StatAST> ParseWhileStat() {
	getNextToken();  // eat the while.

	// while.
	auto While = ParseExpression();
	if (!While)
		return nullptr;

	if (CurTok != DO)
		return LogError("expected do");
	getNextToken();  // eat the Do

	if (CurTok != '{')
		return LogError("expected {");
	getNextToken();  // eat the {
	auto Do = ParseStatement();		
	if (!Do)
		return nullptr;

	if (CurTok != '}')
		return LogError("expected }");
	getNextToken();  // eat the }

	if (CurTok != DONE)
		return LogError("expect DONE in WHILE statement");
	getNextToken();//eat DONE

	return llvm::make_unique<WhileStatAST>(std::move(While), std::move(Do));
}

//解析 赋值语句
static std::unique_ptr<StatAST> ParseAssStat() {
	auto a = ParseIdentifierExpr();
	VariableExprAST* Name = (VariableExprAST*)a.get();
	auto NameV = llvm::make_unique<VariableExprAST>(Name->getName());
	if (!Name)
		return nullptr;
	if (CurTok != ASSIGN_SYMBOL)
		return LogError("need := in assignment statment");
	getNextToken();

	auto Expression = ParseExpression();
	if (!Expression)
		return nullptr;

	return llvm::make_unique<AssStatAST>(std::move(NameV), std::move(Expression));
}


//解析 RETURN
static std::unique_ptr<StatAST> ParseRetStat(){
	getNextToken();
	auto Val = ParseExpression();
	if (!Val)
		return nullptr;

	return llvm::make_unique<RetStatAST>(std::move(Val));
}

//解析变量声明
static std::unique_ptr<StatAST> ParseDecStat() {
	//eat 'VAR'
	getNextToken();

	std::vector<std::string> varNames;
	//保证至少有一个变量的名字
	if (CurTok != VARIABLE) {
		return LogErrorD("expected identifier after VAR");
	}

	while (true)
	{
		varNames.push_back(IdentifierStr);
		//eat VARIABLE
		getNextToken();
		if (CurTok != ',')
			break;
		getNextToken();
		if (CurTok != VARIABLE) {
			return LogErrorD("expected identifier list after VAR");
		}
	}

	auto Body = nullptr;

	return llvm::make_unique<DecStatAST>(std::move(varNames), std::move(Body));
}


//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//
static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
//static std::map<std::string, Value *> NamedValues;
static std::map<std::string, AllocaInst *> NamedValues;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
	const std::string &VarName);



Value *LogErrorV(const char *Str) {
	LogError(Str);
	return nullptr;
}

Function *getFunction(std::string Name) {
	// First, see if the function has already been added to the current module.
	if (auto *F = TheModule->getFunction(Name))
		return F;

	// If not, check whether we can codegen the declaration from some existing
	// prototype.
	auto FI = FunctionProtos.find(Name);
	if (FI != FunctionProtos.end())
		return FI->second->codegen();

	// If no existing prototype exists, return null.
	return nullptr;
}



Value *NumberExprAST::codegen() {
	return ConstantInt::get(TheContext, APInt(32, Val,true));
}

Value *VariableExprAST::codegen() {
	// Look this variable up in the function.
	Value *V = NamedValues[Name];
	if (!V)
		return LogErrorV("Unknown variable name");
	return Builder.CreateLoad(V, Name.c_str());
}

Value *BinaryExprAST::codegen() {

	Value *L = LHS->codegen();
	Value *R = RHS->codegen();
	if (!L || !R)
		return nullptr;

	switch (Op) {
	case '+':
		return Builder.CreateAdd(L, R, "addtmp");
	case '-':
		return Builder.CreateSub(L, R, "subtmp");
	case '*':
		return Builder.CreateMul(L, R, "multmp");
	case '/':
		L = Builder.CreateExactSDiv(L, R, "divtmp");
		// Convert bool 0/1 to int 0 or 1
		return Builder.CreateUIToFP(L, Type::getInt32Ty(TheContext), "booltmp");
	default:
		return LogErrorV("invalid binary operator");
	}

}

Value *CallExprAST::codegen() {
	// Look up the name in the global module table.
	Function *CalleeF = getFunction(Callee);
	if (!CalleeF)
		return LogErrorV("Unknown function referenced");

	// If argument mismatch error.
	if (CalleeF->arg_size() != Args.size())
		return LogErrorV("Incorrect # arguments passed");

	std::vector<Value *> ArgsV;
	for (unsigned i = 0, e = Args.size(); i != e; ++i) {
		ArgsV.push_back(Args[i]->codegen());
		if (!ArgsV.back())
			return nullptr;
	}

	return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::codegen() {
	// Make the function type:  double(double,double) etc.
	std::vector<Type*> Integers(Args.size(),
		Type::getInt32Ty(TheContext));
	FunctionType *FT =
		FunctionType::get(Type::getInt32Ty(TheContext), Integers, false);

	Function *F =
		Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

	// Set names for all arguments.
	unsigned Idx = 0;
	for (auto &Arg : F->args())
		Arg.setName(Args[Idx++]);

	return F;
}

Function *FunctionAST::codegen() {
	// Transfer ownership of the prototype to the FunctionProtos map, but keep a
	// reference to it for use below.
	auto &P = *Proto;
	FunctionProtos[Proto->getName()] = std::move(Proto);
	Function *TheFunction = getFunction(P.getName());
	if (!TheFunction)
		return nullptr;

	// Create a new basic block to start insertion into.
	BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
	Builder.SetInsertPoint(BB);

	// Record the function arguments in the NamedValues map.
	NamedValues.clear();
	for (auto &Arg : TheFunction->args()) {
        // Create an alloca for this variable.
		AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName());

	    // Store the initial value into the alloca.
	    Builder.CreateStore(&Arg, Alloca);

	    // Add arguments to variable symbol table.
	    NamedValues[Arg.getName()] = Alloca;
	}
		
	if (Value *RetVal = Body->codegen()) {
		// Finish off the function.
		Builder.CreateRet(RetVal);

		// Validate the generated code, checking for consistency.
		verifyFunction(*TheFunction);

		return TheFunction;
	}

	TheFunction->eraseFromParent();
	return nullptr;

}

Value *IfStatAST::codegen() {
	Value *CondV = Cond->codegen();
	if (!CondV)
		return nullptr;

	// Convert condition to a bool by comparing non-equal to 0.0.
	CondV = Builder.CreateICmpNE(
		CondV, Builder.getInt32(0), "ifcond");

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	// Create blocks for the then and else cases.  Insert the 'then' block at the
	// end of the function.
	BasicBlock *ThenBB = BasicBlock::Create(TheContext, "then", TheFunction);
	BasicBlock *ElseBB = BasicBlock::Create(TheContext, "else");
	BasicBlock *MergeBB = BasicBlock::Create(TheContext, "ifcont");

	Builder.CreateCondBr(CondV, ThenBB, ElseBB);

	// Emit then value.
	Builder.SetInsertPoint(ThenBB);

	Value *ThenV = Then->codegen();
	if (!ThenV)
		return nullptr;

	Builder.CreateBr(MergeBB);
	// Codegen of 'Then' can change the current block, update ThenBB for the PHI.
	ThenBB = Builder.GetInsertBlock();

	// Emit else block.
	TheFunction->getBasicBlockList().push_back(ElseBB);
	Builder.SetInsertPoint(ElseBB);

	Value *ElseV = Else->codegen();
	if (!ElseV)
		return nullptr;

	Builder.CreateBr(MergeBB);
	// Codegen of 'Else' can change the current block, update ElseBB for the PHI.
	ElseBB = Builder.GetInsertBlock();

	// Emit merge block.
	TheFunction->getBasicBlockList().push_back(MergeBB);
	Builder.SetInsertPoint(MergeBB);
	PHINode *PN = Builder.CreatePHI(Type::getInt32Ty(TheContext), 2, "iftmp");

	PN->addIncoming(ThenV, ThenBB);
	PN->addIncoming(ElseV, ElseBB);
	return PN;
}


Value *WhileStatAST::codegen() {
	Function *TheFunction = Builder.GetInsertBlock()->getParent();
	BasicBlock *LoopBB = BasicBlock::Create(TheContext, "loop", TheFunction);
	BasicBlock *AfterBB = BasicBlock::Create(TheContext, "afterLoop", TheFunction);

	Value *WhileV = While->codegen();
	if (!WhileV)
		return nullptr;
	WhileV = Builder.CreateICmpNE(WhileV, Builder.getInt32(0),
		"inLoop");
	//类似if判断
	Builder.CreateCondBr(WhileV, LoopBB, AfterBB);

	Builder.SetInsertPoint(LoopBB);
	Value *DoV = Do->codegen();
	if (!DoV)
		return nullptr;
	WhileV = Builder.CreateICmpNE(While->codegen(),
		Builder.getInt32(0), "outLoop");
	Builder.CreateCondBr(WhileV, LoopBB, AfterBB);

	Builder.SetInsertPoint(AfterBB);

	return Builder.getInt32(0);
	
}

Value *RetStatAST::codegen() {
	Function *TheFunction = Builder.GetInsertBlock()->getParent();
	if (Value *RetVal = Val->codegen()) {
		Builder.CreateRet(RetVal);
		BasicBlock *afterRet = BasicBlock::Create(TheContext, "afterReturn", TheFunction);
		Builder.SetInsertPoint(afterRet);

		return RetVal;
	}
}

Value *DecStatAST::codegen() {
	std::vector<AllocaInst *> OldBindings;

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
		const std::string &VarName = VarNames[i];

		Value *InitVal = ConstantInt::get(TheContext, APInt(32, 0));

		AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
		Builder.CreateStore(InitVal, Alloca);

		OldBindings.push_back(NamedValues[VarName]);
		NamedValues[VarName] = Alloca;
	}

	return nullptr;
}

Value *AssStatAST::codegen() {
	Value* EValue = Expression->codegen();
	if (!EValue)
		return nullptr;

	Value *Variable = NamedValues[Name->getName()];
	if (!Variable)
		return LogErrorV("Unknown variable name");

	Builder.CreateStore(EValue, Variable);

	return EValue;
}

Value *BlockStatAST::codegen()
{
	for (int i = 0; i < DecList.size(); i++)
	{
		DecList[i]->codegen();
	}
	for (int j = 0; j < StatList.size(); j++)
	{
		StatList[j]->codegen();
	}
	return Builder.getInt32(0); //block always return 0
}

// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
// the function.  This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
	const std::string &VarName) {
	IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
		TheFunction->getEntryBlock().begin());
	return TmpB.CreateAlloca(Type::getInt32Ty(TheContext), nullptr,
		VarName.c_str());
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//
static void InitializeModuleAndPassManager() {
	// Open a new module.
	TheModule = llvm::make_unique<Module>("my cool jit", TheContext);
	TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());

	// Create a new pass manager attached to it.
	TheFPM = llvm::make_unique<legacy::FunctionPassManager>(TheModule.get());

	// Promote allocas to registers.
	TheFPM->add(createPromoteMemoryToRegisterPass());
	// Do simple "peephole" optimizations and bit-twiddling optzns.
	TheFPM->add(createInstructionCombiningPass());
	// Reassociate expressions.
	TheFPM->add(createReassociatePass());
	
	// Eliminate Common SubExpressions.
	TheFPM->add(createGVNPass());
	// Simplify the control flow graph (deleting unreachable blocks, etc).
	TheFPM->add(createCFGSimplificationPass());

	TheFPM->doInitialization();
}

static void HandleDefinition() {
	if (auto FnAST = ParseFunc()) {
		if (auto *FnIR = FnAST->codegen()) {
			fprintf(stderr, "Read function definition:");
			FnIR->print(errs());
			fprintf(stderr, "\n");
			TheJIT->addModule(std::move(TheModule));
			InitializeModuleAndPassManager();
		}
	}
	else {
		// Skip token for error recovery.
		getNextToken();
	}
}



static void HandleTopLevelExpression() {
	// Evaluate a top-level expression into an anonymous function.
	if (auto FnAST = ParseTopLevelExpr()) {
		if (auto *FnIR =  FnAST->codegen()) {
			fprintf(stderr, "Read function call:");
			FnIR->print(errs());
			fprintf(stderr, "\n");
			// JIT the module containing the anonymous expression, keeping a handle so
			// we can free it later.
			auto H = TheJIT->addModule(std::move(TheModule));
			InitializeModuleAndPassManager();

			// Search the JIT for the __anon_expr symbol.
			auto ExprSymbol = TheJIT->findSymbol("__anon_expr");
			assert(ExprSymbol && "Function not found");

			// Get the symbol's address and cast it to the right type (takes no
			// arguments, returns a double) so we can call it as a native function.
			
			/*int(*FP)() = (int(*)())cantFail(ExprSymbol.getAddress());
			fprintf(stderr, "Evaluated to %d\n", FP());*/

			// Delete the anonymous expression module from the JIT.
			TheJIT->removeModule(H);
		}
	}
	else {
		// Skip token for error recovery.
		getNextToken();
	}
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
	while (true) {
		fprintf(stderr, "ready> ");
		switch (CurTok) {
		case tok_eof:
			return;
		case ';': // ignore top-level semicolons.
			getNextToken();
			break;
		case FUNC:
			HandleDefinition();
			break;
		default:
			HandleTopLevelExpression();
			break;
		}
	}
}

//"Library" functions that can be "extern'd" from user code.
#ifdef LLVM_ON_WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
	fputc((char)X, stderr);
	return 0;
}

// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
	fprintf(stderr, "%f\n", X);
	return 0;
}


#endif

