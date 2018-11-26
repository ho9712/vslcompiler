#ifndef __LEXER_H__
#define __LEXER_H__
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
// Lexer
//===----------------------------------------------------------------------===//

enum Token
{
	tok_eof = -1,
	FUNC = -2,		 //关键字'FUNC'
	VARIABLE = -3,	      //标识符 
	ASSIGN_SYMBOL = -4,	 // 赋值
	INTEGER = -5,		 //整数
	PRINT = -6,			 // 关键字'PRINT' 
	TEXT = -7,			 // 字符常量			 
	RETURN = -8,         //关键字
	CONTINUE = -9,
	IF = -10,
	THEN = -11,
	ELSE = -12,
	FI = -13,
	WHILE = -14,
	DO = -15,
	DONE = -16,
	VAR = -17,
};

static std::string IdentifierStr; //标识符
static int NumVal;  //整数

//返回字符类型
static int gettok() {
	static int LastChar = ' ';

	//删除空格
	while (isspace(LastChar))
		LastChar = getchar();

	if (isalpha(LastChar)) { // 标识符: （{lc_letter}|{uc_letter}）({lc_letter}|{uc_letter}|{digit})*
		IdentifierStr = LastChar;
		while (isalnum((LastChar = getchar())))
			IdentifierStr += LastChar;


		if (IdentifierStr == "FUNC")
			return FUNC;
		if (IdentifierStr == "PRINT")
			return PRINT;
		if (IdentifierStr == "RETURN")
			return RETURN;
		if (IdentifierStr == "CONTINUE")
			return CONTINUE;
		if (IdentifierStr == "IF")
			return IF;
		if (IdentifierStr == "THEN")
			return THEN;
		if (IdentifierStr == "ELSE")
			return ELSE;
		if (IdentifierStr == "FI")
			return FI;
		if (IdentifierStr == "WHILE")
			return WHILE;
		if (IdentifierStr == "DO")
			return DO;
		if (IdentifierStr == "DONE")
			return DONE;
		if (IdentifierStr == "VAR")
			return VAR;

		return VARIABLE;//自定义变量名
	}

	if (isdigit(LastChar)) {//整数:{digit}+
		std::string NumStr;
		do {
			NumStr += LastChar;
			LastChar = getchar();
		} while (isdigit(LastChar));

		NumVal = atoi(IdentifierStr.c_str());
		return INTEGER;
	}


	if (LastChar == ':' && (LastChar = getchar()) == '=') {//赋值:  ":="	
		LastChar = getchar();
		return ASSIGN_SYMBOL;
	}

	if (LastChar == '\"' || LastChar == '\n') {//text:  \"({ascii_char}|{escaped_char})*\" 
		do
		{
			IdentifierStr += LastChar;
			LastChar = getchar();
		} while (LastChar != '\"' || LastChar != '\n');
		return TEXT;
	}



	//结束符
	if (LastChar == EOF)
		return tok_eof;

	//如果都不是返回当前字符
	int ThisChar = LastChar;
	LastChar = getchar();
	return ThisChar;
}

#endif