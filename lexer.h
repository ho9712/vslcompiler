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
	FUNC = -2,		 //�ؼ���'FUNC'
	VARIABLE = -3,	      //��ʶ�� 
	ASSIGN_SYMBOL = -4,	 // ��ֵ
	INTEGER = -5,		 //����
	PRINT = -6,			 // �ؼ���'PRINT' 
	TEXT = -7,			 // �ַ�����			 
	RETURN = -8,         //�ؼ���
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

static std::string IdentifierStr; //��ʶ��
static int NumVal;  //����

//�����ַ�����
static int gettok() {
	static int LastChar = ' ';

	//ɾ���ո�
	while (isspace(LastChar))
		LastChar = getchar();

	if (isalpha(LastChar)) { // ��ʶ��: ��{lc_letter}|{uc_letter}��({lc_letter}|{uc_letter}|{digit})*
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

		return VARIABLE;//�Զ��������
	}

	if (isdigit(LastChar)) {//����:{digit}+
		std::string NumStr;
		do {
			NumStr += LastChar;
			LastChar = getchar();
		} while (isdigit(LastChar));

		NumVal = atoi(IdentifierStr.c_str());
		return INTEGER;
	}


	if (LastChar == ':' && (LastChar = getchar()) == '=') {//��ֵ:  ":="	
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



	//������
	if (LastChar == EOF)
		return tok_eof;

	//��������Ƿ��ص�ǰ�ַ�
	int ThisChar = LastChar;
	LastChar = getchar();
	return ThisChar;
}

#endif