#include "lexer.h"
#include "parser.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"



int main() {
	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();

	// Install standard binary operators.
	// 1 is lowest precedence.
	
	BinopPrecedence['+'] = 20;
	BinopPrecedence['-'] = 20;
	BinopPrecedence['*'] = 40;
	BinopPrecedence['/'] = 40;// highest.

							  // Prime the first token.
	fprintf(stderr, "ready> ");
	getNextToken();

	TheJIT = llvm::make_unique<KaleidoscopeJIT>();

	InitializeModuleAndPassManager();
	// Run the main "interpreter loop" now.
	MainLoop();

	auto TargetTriple = sys::getDefaultTargetTriple();
    TheModule->setTargetTriple(TargetTriple);
	// Initialize the target registry etc.
	InitializeAllTargetInfos();
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmParsers();
	InitializeAllAsmPrinters();
	
	

	std::string Error;
	auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

	// Print an error and exit if we couldn't find the requested target.
	// This generally occurs if we've forgotten to initialise the
	// TargetRegistry or we have a bogus target triple.
	if (!Target) {
		errs() << Error;
		return 1;
	}

	auto CPU = "generic";
	auto Features = "";

	TargetOptions opt;
	auto RM = Optional<Reloc::Model>();
	auto TheTargetMachine =
		Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

	TheModule->setDataLayout(TheTargetMachine->createDataLayout());

	auto Filename = "output.o";
	std::error_code EC;
	raw_fd_ostream dest(Filename, EC, sys::fs::F_None);

	if (EC) {
		errs() << "Could not open file: " << EC.message();
		return 1;
	}

	legacy::PassManager pass;
	auto FileType = TargetMachine::CGFT_ObjectFile;

	if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
		errs() << "TheTargetMachine can't emit a file of this type";
		return 1;
	}

	pass.run(*TheModule);
	dest.flush();

	outs() << "Wrote " << Filename << "\n";

	
	return 0;
}
