#ifndef __PARSER_H__
#define __PARSER_H__
#include "Lexer.h"
#include "llvm/ADT/STLExtras.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>
//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//
namespace {
	//���ʽ
	class ExprAST {
	public:
		virtual ~ExprAST() = default;
	};
	

	//����
	class NumberExprAST : public ExprAST {
		int Val;
	public:
		NumberExprAST(int Val) : Val(Val) {}
	};

	//����
	class VariableExprAST : public ExprAST {
		std::string Name;
	public:
		VariableExprAST(const std::string &Name) : Name(Name) {}
	};

	//�����
	class BinaryExprAST : public ExprAST {
		char Op;
		std::unique_ptr<ExprAST> LHS, RHS;

	public:
		BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
			std::unique_ptr<ExprAST> RHS)
			: Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
		
	};

	//����ԭ��
	class PrototypeAST {
		std::string Name;
		std::vector<std::string> Args;

	public:
		PrototypeAST(const std::string &Name, std::vector<std::string> Args)
			: Name(Name), Args(std::move(Args)) {}

		const std::string &getName() const { return Name; }

	};

	//����
	class FunctionAST {
		std::unique_ptr<PrototypeAST> Proto;
		std::unique_ptr<ExprAST> Body;

	public:
		FunctionAST(std::unique_ptr<PrototypeAST> Proto,
			std::unique_ptr<ExprAST> Body)
			: Proto(std::move(Proto)), Body(std::move(Body)) {}

	};

	//��������
	class CallExprAST : public ExprAST {
		std::string Callee;
		std::vector<std::unique_ptr<ExprAST>> Args;

	public:
		CallExprAST(const std::string &Callee,
			std::vector<std::unique_ptr<ExprAST>> Args)
			: Callee(Callee), Args(std::move(Args)) {}
	};

	//����ĳ����﷨��
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

//��������ȼ�
static std::map<char, int> BinopPrecedence;

///�����������ȼ�
static int GetTokPrecedence() {
	if (!isascii(CurTok))
		return -1;

	// ȷ�������
	int TokPrec = BinopPrecedence[CurTok];
	if (TokPrec <= 0)
		return -1;
	return TokPrec;
}

//������
std::unique_ptr<ExprAST> LogError(const char *Str) {
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
	LogError(Str);
	return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

//��������
static std::unique_ptr<ExprAST> ParseNumberExpr() {
	auto Result = llvm::make_unique<NumberExprAST>(NumVal);
	getNextToken(); // consume the number
	return std::move(Result);
}

//��������
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

/// �������ʽ
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
	std::string IdName = IdentifierStr;

	getNextToken(); 

	if (CurTok != '(') 
		return llvm::make_unique<VariableExprAST>(IdName);

	//����
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

//�����ַ����ࣺ��ʶ�� ���� ����
static std::unique_ptr<ExprAST> ParsePrimary() {
	switch (CurTok) {
	case VARIABLE:
		return ParseIdentifierExpr();
	case INTEGER:
		return ParseNumberExpr();
	case '(':
		return ParseParenExpr();
	default:
		return LogError("unknown token when expecting an expression");
	}
}

///������������ı��ʽ
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
	std::unique_ptr<ExprAST> LHS) {
	// ��ѯ�Ƿ��������������ȼ�
	while (true) {
		int TokPrec = GetTokPrecedence();

		//�ж��Ƿ��Ҳ��������������������ȼ�
		if (TokPrec < ExprPrec)
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

		// ����������
		LHS = llvm::make_unique<BinaryExprAST>(BinOp, std::move(LHS),
			std::move(RHS));
	}
}

/// �������ʽ
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() {
	auto LHS = ParsePrimary();
	if (!LHS)
		return nullptr;

	return ParseBinOpRHS(0, std::move(LHS));
}

///����ԭ��
///   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
	if (CurTok != VARIABLE)
		return LogErrorP("Expected function name in prototype");

	std::string FnName = IdentifierStr;
	getNextToken();

	if (CurTok != '(')
		return LogErrorP("Expected '(' in prototype");
	getNextToken();

	std::vector<std::string> ArgNames;
	while (getNextToken() == VARIABLE) {
		ArgNames.push_back(IdentifierStr);
		getNextToken();
	}
	if (CurTok != ')')
		return LogErrorP("Expected ')' in prototype");


	// success.
	getNextToken(); // eat ')'.

	return llvm::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

//�������� ::= FUNC prototype '{' statement '}'
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
	if (E)
		return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	return nullptr;
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

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//

static void HandleDefinition() {
	if (ParseFunc()) {
		fprintf(stderr, "Parsed a function definition.\n");
	}
	else {
		// Skip token for error recovery.
		getNextToken();
	}
}


static void HandleTopLevelExpression() {
	// Evaluate a top-level expression into an anonymous function.

	if (ParseTopLevelExpr()) {
		fprintf(stderr, "Parsed a top-level expr\n");
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



#endif

