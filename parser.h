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
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include "../include/KaleidoscopeJIT.h"


using namespace llvm;
using namespace llvm::orc;
//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//
namespace {
	//表达式
	class ExprAST {
	public:
		virtual ~ExprAST() = default;
		virtual Value *codegen() = 0;
	};
	

	//整数
	class NumberExprAST : public ExprAST {
		int Val;
	public:
		NumberExprAST(int Val) : Val(Val) {}
		Value *codegen() override;
	};

	//变量
	class VariableExprAST : public ExprAST {
		std::string Name;
	public:
		VariableExprAST(const std::string &Name) : Name(Name) {}
		Value *codegen() override;
	};

	//运算符
	class BinaryExprAST : public ExprAST {
		char Op;
		std::unique_ptr<ExprAST> LHS, RHS;

	public:
		BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
			std::unique_ptr<ExprAST> RHS)
			: Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
		Value *codegen() override;
		
	};

	//函数原型
	class PrototypeAST {
		std::string Name;
		std::vector<std::string> Args;

	public:
		PrototypeAST(const std::string &Name, std::vector<std::string> Args)
			: Name(Name), Args(std::move(Args)) {}

		Function *codegen();
		const std::string &getName() const { return Name; }

	};

	//函数
	class FunctionAST {
		std::unique_ptr<PrototypeAST> Proto;
		std::unique_ptr<ExprAST> Body;

	public:
		FunctionAST(std::unique_ptr<PrototypeAST> Proto,
			std::unique_ptr<ExprAST> Body)
			: Proto(std::move(Proto)), Body(std::move(Body)) {}
		Function *codegen();
	};

	//调用
	class CallExprAST : public ExprAST {
		std::string Callee;
		std::vector<std::unique_ptr<ExprAST>> Args;

	public:
		CallExprAST(const std::string &Callee,
			std::vector<std::unique_ptr<ExprAST>> Args)
			: Callee(Callee), Args(std::move(Args)) {}
		Value *codegen() override;
	};

	//变量声明
	class VarAST {
		std::vector<std::string> Vars;

	public:
		VarAST(std::vector<std::string> Vars)
			:Vars(std::move(Vars)) {}
	};

	//变量赋值
	class AssignStatAST : public StatAST {
		std::unique_ptr<ExprAST> Exp;
		std::string Name;
	public:
		AssignStatAST(const std::string &Name, std::unique_ptr<ExprAST> Exp)
			:Name(Name), Exp(std::move(Exp)) {}
		Value *codegen() override;
	};

	///PrintItem
	class PrintItemAST {

	public:
		virtual ~PrintItemAST() = default;
	};

	///ExpPrintItem
	class ExpPrintItemAST :public PrintItemAST {
		std::unique_ptr<ExprAST> Exp;
		static const int type = 0;
	public:
		ExpPrintItemAST(std::unique_ptr<ExprAST> Exp)
			:Exp(std::move(Exp)) {}
	};

	///TextPrintItem
	class TextPrintItemAST :public PrintItemAST {
		std::string Text;
		static const int type = 1;
	public:
		TextPrintItemAST(std::string Text)
			:Text(Text) {}
	};

	///Print语句
	class PrintStatAST : public StatAST {
		std::vector<std::unique_ptr<PrintItemAST>> Items;
	public:
		PrintStatAST(std::vector<std::unique_ptr<PrintItemAST>> Items)
			:Items(std::move(Items)) {}
		Value *codegen() override;
	};

	//Return语句
	class ReturnStatAST : public StatAST {
		std::unique_ptr<ExprAST> Exp;

	public:
		ReturnStatAST(std::unique_ptr<ExprAST> Exp)
			:Exp(std::move(Exp)) {}
		bool isRet() {
			return true;
		}
		Value *codegen() override;
	};

	///If语句
	class IfStatAST : public StatAST {
		std::unique_ptr<ExprAST> Exp;
		std::unique_ptr<StatAST> tStat;
		std::unique_ptr<StatAST> eStat;
	public:
		IfStatAST(std::unique_ptr<ExprAST> Exp, std::unique_ptr<StatAST> tStat)
			:Exp(std::move(Exp)), tStat(std::move(tStat)) {
			eStat = nullptr;
		}
		IfStatAST(std::unique_ptr<ExprAST> Exp,
			std::unique_ptr<StatAST> tStat, std::unique_ptr<StatAST> eStat)
			:Exp(std::move(Exp)), tStat(std::move(tStat)), eStat(std::move(eStat)) {}
		Value *codegen() override;
	};

	///While语句
	class WhileStatAST : public StatAST {
		std::unique_ptr<ExprAST> Exp;
		std::unique_ptr<StatAST> Stat;

	public:
		WhileStatAST(std::unique_ptr<ExprAST> Exp,
			std::unique_ptr<StatAST> Stat)
			:Exp(std::move(Exp)), Stat(std::move(Stat)) {}
		Value *codegen() override;
	};

	//If条件
	class IfExprAST : public ExprAST {
		std::unique_ptr<ExprAST> Cond, Then, Else;

	public:
		IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
			std::unique_ptr<ExprAST> Else)
			: Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

		Value *codegen() override;
	};

	//程序的抽象语法树
	class ProgramAST {
		std::vector<std::unique_ptr<FunctionAST>> funcs;

	public:
		ProgramAST(std::vector<std::unique_ptr<FunctionAST>> funcs)
			:funcs(std::move(funcs)) {}
	};

	
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
std::unique_ptr<ExprAST> LogError(const char *Str) {
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
	LogError(Str);
	return nullptr;
}
std::unique_ptr<FunctionAST> LogErrorF(const char *Str) {
	LogError(Str);
	return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();
static std::unique_ptr<ExprAST> ParseIfExpr();

//解析数字
static std::unique_ptr<ExprAST> ParseNumberExpr() {
	auto Result = llvm::make_unique<NumberExprAST>(NumVal);
	getNextToken(); // consume the number
	return std::move(Result);
}

//解析括号
static std::unique_ptr<ExprAST> ParseParenExpr() {
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
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
	std::string IdName = IdentifierStr;

	getNextToken(); 

	if (CurTok != '(') 
		return llvm::make_unique<VariableExprAST>(IdName);

	//调用
	getNextToken();
	std::vector<std::unique_ptr<ExprAST>> Args;
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
static std::unique_ptr<ExprAST> ParsePrimary() {
	switch (CurTok) {
	case VARIABLE:
		return ParseIdentifierExpr();
	case INTEGER:
		return ParseNumberExpr();
	case '(':
		return ParseParenExpr();
	case IF:
		return  ParseIfExpr();
	default:
		return LogError("unknown token when expecting an expression");
	}
}

///解析带运算符的表达式
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
	std::unique_ptr<ExprAST> LHS) {
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
static std::unique_ptr<ExprAST> ParseExpression() {
	auto LHS = ParsePrimary();
	if (!LHS)
		return nullptr;
	return ParseBinOpRHS(0, std::move(LHS));
}


///变量声明 parse
static  std::unique_ptr<VarAST> ParseVar() {
	if (CurTok != VAR)
		return nullptr;
	std::vector<std::string> Vars;
	do {
		if (getNextToken() == VARIABLE) {
			std::string var = IdentifierStr;
			Vars.push_back(var);
			getNextToken();
		}
		else {
			LogError("Expected VARIABLE after VAR");
			return nullptr;
		}
	} while (CurTok == ',');
	return llvm::make_unique<VarAST>(std::move(Vars));
}

///变量赋值 parse
static std::unique_ptr<StatAST> ParseAssignStat() {
	std::string Vname = IdentifierStr;	//得到变量名
	getNextToken();
	if (CurTok != ASSIGN_SYMBOL) {
		LogError("Expected ':=' in prototype");
		return nullptr;
	}
	getNextToken();
	if (auto E = ParseExpression())
		return llvm::make_unique<AssignStatAST>(Vname, std::move(E));
	return nullptr;
}

///Print语句 parse
static  std::unique_ptr<StatAST> ParsePrintStat() {
	std::vector<std::unique_ptr<PrintItemAST>> Items;
	do {
		if (getNextToken() == TEXT) {
			Items.push_back(llvm::make_unique<TextPrintItemAST>(IdentifierStr));
			getNextToken();
		}
		else {
			if (auto E = ParseExpression())
				Items.push_back(llvm::make_unique<ExpPrintItemAST>(std::move(E)));
			else
			{
				return nullptr;
			}
		}
	} while (CurTok == ',');
	return llvm::make_unique<PrintStatAST>(std::move(Items));
}

///Return语句 parse
static  std::unique_ptr<StatAST> ParseReturnStat() {
	getNextToken();	//eat RETURN
	if (auto E = ParseExpression())
		return llvm::make_unique<ReturnStatAST>(std::move(E));
	return nullptr;
}

static std::unique_ptr<StatAST> ParseStatment();

///If语句 parse
static  std::unique_ptr<StatAST> ParseIfStat() {
	getNextToken();	//eat IF
	if (auto E = ParseExpression()) {
		if (CurTok != THEN) {
			LogError("Expected 'THEN'");
			return nullptr;
		}
		getNextToken();	//eat THEN
		if (auto tS = ParseStatment()) {
			//getNextToken();
			if (CurTok == FI) {
				getNextToken();
				return llvm::make_unique<IfStatAST>(std::move(E), std::move(tS));
			}
			else if (CurTok == ELSE) {
				if (auto eS = ParseStatment()) {
					if (CurTok == FI) {
						getNextToken();
						return llvm::make_unique<IfStatAST>(std::move(E), std::move(tS), std::move(eS));
					}
					else
					{
						LogError("Expected 'FI'");
						return nullptr;
					}
				}
				else
				{
					LogError("Expected Statement");
					return nullptr;
				}
			}
			else
			{
				LogError("Expected 'ELSE' or 'FI'");
				return nullptr;
			}
		}
		return nullptr;
	}
	return nullptr;
}

///While语句 parse	
static  std::unique_ptr<StatAST> ParseWhileStat() {
	getNextToken();	//eat WHILE
	if (auto E = ParseExpression()) {
		if (CurTok != DO) {
			LogError("Expected 'DO'");
			return nullptr;
		}
		getNextToken();	//eat DO
		if (auto tS = ParseStatment()) {
			if (CurTok == DONE) {
				getNextToken();
				return llvm::make_unique<WhileStatAST>(std::move(E), std::move(tS));
			}
			else
			{
				LogError("Expected 'DONE'");
				return nullptr;
			}
		}
		return nullptr;
	}
	return nullptr;
}


static std::unique_ptr<StatAST> ParseStatment() {
	switch (CurTok)
	{
	case VARIABLE:
		return ParseAssignStat();
	case PRINT:
		return ParsePrintStat();
	case RETURN:
		return ParseReturnStat();
	case IF:
		return ParseIfStat();
	case WHILE:
		return ParseWhileStat();
	default:
		break;
	}
	return nullptr;
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
	if (CurTok != '{')
	{
		LogErrorP("Expected '{' in function");
		return nullptr;
	}
	getNextToken();

	auto E = ParseExpression();
	if (!E)	
		return nullptr;
	if (CurTok != '}')
	{
		LogErrorP("Expected '}' in function");
		return nullptr;
	}
	getNextToken();

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

//parse if语句
static std::unique_ptr<ExprAST> ParseIfExpr() {
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

	std::unique_ptr<ExprAST> Else = nullptr;
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

	return llvm::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
		std::move(Else));
}
//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//
static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value *> NamedValues;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;


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
	return V;
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
	for (auto &Arg : TheFunction->args())
		NamedValues[Arg.getName()] = &Arg;

	if (Value *RetVal = Body->codegen()) {
		// Finish off the function.
		Builder.CreateRet(RetVal);

		// Validate the generated code, checking for consistency.
		verifyFunction(*TheFunction);

		// Run the optimizer on the function.
		TheFPM->run(*TheFunction);

		return TheFunction;
	}

	// Error reading body, remove function.
	TheFunction->eraseFromParent();
	return nullptr;
}

bool StatAST::isRet()
{
	return false;
}

Value * ReturnStatAST::codegen()
{
	if (Value *RetVal = Exp->codegen()) {
		return Builder.CreateRet(RetVal);
	}
	return nullptr;
}

Value * IfStatAST::codegen()
{
	Value *CondV = Exp->codegen();
	if (!CondV)
		return nullptr;

	// Convert condition to a bool by comparing non-equal to 0.0.
	CondV = Builder.CreateFCmpONE(
		CondV, ConstantFP::get(TheContext, APFloat(0.0)), "ifcond");

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	// Create blocks for the then and else cases.  Insert the 'then' block at the
	// end of the function.
	BasicBlock *ThenBB = BasicBlock::Create(TheContext, "then", TheFunction);
	BasicBlock *ElseBB = BasicBlock::Create(TheContext, "else");
	BasicBlock *MergeBB = BasicBlock::Create(TheContext, "ifcont");

	Builder.CreateCondBr(CondV, ThenBB, ElseBB);

	// Emit then value.
	Builder.SetInsertPoint(ThenBB);

	Value *ThenV = tStat->codegen();
	if (!ThenV)
		return nullptr;

	Builder.CreateBr(MergeBB);
	// Codegen of 'Then' can change the current block, update ThenBB for the PHI.
	ThenBB = Builder.GetInsertBlock();

	// Emit else block.
	TheFunction->getBasicBlockList().push_back(ElseBB);
	Builder.SetInsertPoint(ElseBB);

	Value *ElseV = eStat->codegen();
	if (!ElseV)
		return nullptr;

	Builder.CreateBr(MergeBB);
	// Codegen of 'Else' can change the current block, update ElseBB for the PHI.
	ElseBB = Builder.GetInsertBlock();

	// Emit merge block.
	TheFunction->getBasicBlockList().push_back(MergeBB);
	Builder.SetInsertPoint(MergeBB);
	PHINode *PN = Builder.CreatePHI(Type::getDoubleTy(TheContext), 2, "iftmp");

	PN->addIncoming(ThenV, ThenBB);
	PN->addIncoming(ElseV, ElseBB);
	return PN;

}

Value * ::WhileStatAST::codegen() {
	Value *EndCond = Exp->codegen();
	if (!EndCond)
		return nullptr;

	EndCond = Builder.CreateFCmpONE(
		EndCond, ConstantFP::get(TheContext, APFloat(0.0)), "whilecond");

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	BasicBlock *PreheaderBB = Builder.GetInsertBlock();

	BasicBlock *LoopBB = BasicBlock::Create(TheContext, "loop", TheFunction);

	Builder.CreateBr(LoopBB);

	Builder.SetInsertPoint(LoopBB);

	PHINode *Variable =
		Builder.CreatePHI(Type::getDoubleTy(TheContext), 2, "loopend");

	Variable->addIncoming(EndCond, PreheaderBB);

	if (!Stat->codegen())
		return nullptr;

	EndCond = Exp->codegen();
	if (!EndCond)
		return nullptr;

	EndCond = Builder.CreateFCmpONE(
		EndCond, ConstantFP::get(TheContext, APFloat(0.0)), "whilecond");

	BasicBlock *LoopEndBB = Builder.GetInsertBlock();

	BasicBlock *AfterBB =
		BasicBlock::Create(TheContext, "afterloop", TheFunction);

	Builder.CreateCondBr(EndCond, LoopBB, AfterBB);

	Builder.SetInsertPoint(AfterBB);

	Variable->addIncoming(EndCond, LoopEndBB);

	return Constant::getNullValue(Type::getDoubleTy(TheContext));
}

Value * PrintStatAST::codegen()
{
	return nullptr;
}

/*  IF表达式
Value *IfExprAST::codegen() {
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
*/

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//
static void InitializeModuleAndPassManager() {
	// Open a new module.
	TheModule = llvm::make_unique<Module>("my cool jit", TheContext);
	TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());

	// Create a new pass manager attached to it.
	TheFPM = llvm::make_unique<legacy::FunctionPassManager>(TheModule.get());


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
		if (FnAST->codegen()) {
			// JIT the module containing the anonymous expression, keeping a handle so
			// we can free it later.
			auto H = TheJIT->addModule(std::move(TheModule));
			InitializeModuleAndPassManager();

			// Search the JIT for the __anon_expr symbol.
			auto ExprSymbol = TheJIT->findSymbol("__anon_expr");
			assert(ExprSymbol && "Function not found");

			// Get the symbol's address and cast it to the right type (takes no
			// arguments, returns a double) so we can call it as a native function.
			
			int(*FP)() = (int(*)())cantFail(ExprSymbol.getAddress());
			fprintf(stderr, "Evaluated to %d\n", FP());

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

