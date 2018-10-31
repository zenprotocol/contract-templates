
contract-templates: AssemblyInfo.fs ASTUtils.fs ExtractParams.fs ModifyAST.fs ParametersData.fs Program.fs WitheredAST.fs
	msbuild contract-templates.sln

run:
	./bin/Debug/contract_templates.exe --help

run-args:
	./bin/Debug/contract_templates.exe -e Foo.fst -m Bar.fst price 12345
