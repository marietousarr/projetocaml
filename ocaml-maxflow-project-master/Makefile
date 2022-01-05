
build:
	@echo "\n==== COMPILING ====\n"
	ocamlbuild ftest.native

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n==== EXECUTING ====\n"
	./ftest.native graphs/graph2 0 10 outfile
	@echo "\n==== RESULT ==== (content of outfile) \n"
	@cat outfile
	dot -Tsvg graphs/result.gv > graphs/result.svg
	@echo "\n==== OPEN /graphs/result.svg TO SEE THE FINAL GRAPH =====\n"	

maxflow_of: build
	@echo "\n==== EXECUTING ====\n"
	./ftest.native $(filter-out $@, $(MAKECMDGOALS)) outfile
	@echo "\n==== RESULT ==== (content of outfile) \n"
	@cat outfile
	dot -Tsvg graphs/result.gv > graphs/result.svg
	@echo "\n==== OPEN /graphs/result.svg TO SEE THE FINAL GRAPH =====\n"	


clean:
	-rm -rf _build/
	-rm ftest.native
	-rm *.svg
	-rm *.gv
